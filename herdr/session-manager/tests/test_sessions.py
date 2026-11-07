import copy
import importlib.machinery
import importlib.util
import pathlib
import tempfile
import unittest
from unittest.mock import patch

path=pathlib.Path(__file__).parents[2]/'.local/bin/herdr-session'
loader=importlib.machinery.SourceFileLoader('sessions',str(path))
spec=importlib.util.spec_from_loader(loader.name,loader);m=importlib.util.module_from_spec(spec);loader.exec_module(m)

def doc(address='t1'):
 return {'spaces':[{'id':'s1','name':'Work','tabs':[{'id':'tab1','name':'Named','root':{'id':'root','type':'row','children':[{'id':'pane1','type':'block','content':{'terminal':address}}]}}]}]}
def snap(cwd):
 return {'schema':1,'id':'snapshot','saved_at':'now','boot_id':'old-boot','document':doc(),'warnings':[],
  'terminals':{'t1':{'address':'t1','space':'s1','tab':'tab1','pane':'pane1','cwd':cwd,'agent':'codex','ref':'11111111-1111-1111-1111-111111111111','created_ms':1,'pid':10}}}

class Tests(unittest.TestCase):
 def auto_probe(self, terminal, boot='old-boot'):
  def rpc(method,params):
   if method=='doc.get':return {'version':1,'value':doc()}
   if method=='terminal.get':return terminal
   raise AssertionError('Unexpected mutation: '+method)
  with patch.object(m,'STATE',pathlib.Path(self.temp.name)),patch.object(m,'rpc',rpc),patch.object(m,'boot_id',return_value=boot):
   return m.restore(self.s,True,automatic=True)
 def test_auto_restore_leaves_running_agent_alone(self):
  result=self.auto_probe({'state':'live','pid':10,'created_ms':1,'status':{'app':'codex'}})
  self.assertEqual(result['actions'],[])
 def test_auto_restore_does_not_resurrect_closed_terminal(self):
  result=self.auto_probe({'state':'exited','pid':10,'created_ms':1})
  self.assertEqual(result['actions'],[])
 def test_auto_restore_does_not_replace_reused_address(self):
  result=self.auto_probe({'state':'live','pid':20,'created_ms':2})
  self.assertEqual(result['actions'],[])
 def test_auto_restore_rechecks_after_core_restart_on_same_boot(self):
  m.atomic_json(pathlib.Path(self.temp.name)/'restored.json',{'snapshot':self.s['id'],'boot_id':'old-boot'})
  terminal={'state':'live','pid':20,'created_ms':1,'argv':['/bin/zsh','-l']}
  with patch.object(m,'agent_launch',side_effect=m.SessionError('preflight reached')):
   with self.assertRaisesRegex(m.SessionError,'preflight reached'):self.auto_probe(terminal)
 def test_select_archive_by_id_and_keep_restored_addresses(self):
  with tempfile.TemporaryDirectory() as temp,patch.object(m,'STATE',pathlib.Path(temp)):
   older=snap(temp);older.update(id='older',saved_at='2026-11-01')
   newer=snap(temp);newer.update(id='newer',saved_at='2026-11-02')
   m.atomic_json(m.STATE/'snapshots/one.json',older)
   m.atomic_json(m.STATE/'snapshots/two.json',newer)
   active=copy.deepcopy(newer);active['document']=doc('t9')
   m.atomic_json(m.STATE/'latest.json',active)
   self.assertEqual([s['id'] for s in m.saved_snapshots()],['newer','older'])
   self.assertEqual(m.selected_snapshot('older'),older)
   self.assertEqual(m.selected_snapshot('newer')['document'],doc('t9'))
   with self.assertRaises(m.SessionError):m.selected_snapshot('../../missing')
 def test_login_shell_path_is_forwarded_for_mise_node(self):
  result=m.subprocess.CompletedProcess([],0,'startup output\n__HERDR_SESSION_PATH__/mise/node/bin:/usr/bin\n','')
  with patch.object(m.shutil,'which',side_effect=[None,'/mise/node/bin/pi']),patch.object(m.subprocess,'run',return_value=result):
   argv=m.agent_launch(['pi','--session','/session.jsonl'],'/tmp')
  self.assertEqual(argv,['/usr/bin/env','PATH=/mise/node/bin:/usr/bin','/mise/node/bin/pi','--session','/session.jsonl'])
 def test_missing_executable_fails_before_starting_any_terminal(self):
  calls=[]
  def rpc(method,params):
   calls.append(method)
   if method=='doc.get':return {'version':1,'value':doc()}
   raise m.SessionError('not found')
  with tempfile.TemporaryDirectory() as temp,patch.object(m,'STATE',pathlib.Path(temp)),patch.object(m,'rpc',rpc),patch.object(m,'boot_id',return_value='new-boot'),patch.object(m,'agent_launch',side_effect=m.SessionError('missing executable')):
   with self.assertRaises(m.SessionError):m.restore(self.s,True)
  self.assertNotIn('terminal.run',calls)
 def test_retention_keeps_three_newest_archives_and_latest(self):
  with tempfile.TemporaryDirectory() as temp:
   state=pathlib.Path(temp);archives=state/'snapshots';archives.mkdir()
   for day in range(1,6):(archives/f'2026-11-0{day}T00-00-00.json').write_text('{}')
   (state/'latest.json').write_text('{"active":true}')
   (archives/'notes.txt').write_text('keep')
   with patch.object(m,'STATE',state):m.prune_snapshots()
   self.assertEqual(sorted(p.name for p in archives.glob('*.json')),[f'2026-11-0{day}T00-00-00.json' for day in (3,4,5)])
   self.assertEqual(m.read_json(state/'latest.json'),{'active':True})
   self.assertTrue((archives/'notes.txt').exists())
 def setUp(self):self.temp=tempfile.TemporaryDirectory();self.addCleanup(self.temp.cleanup);self.s=snap(self.temp.name)
 def test_live_agent_is_never_restarted(self):
  live={'t1':{'state':'live','pid':10,'created_ms':1,'status':{'app':'codex'}}}
  self.assertEqual(m.plan_restore(self.s,doc(),live,'old-boot')[0]['action'],'keep-live')
 def test_unresolved_session_blocks_reboot_restore(self):
  self.s['terminals']['t1']['ref']=None
  with self.assertRaises(m.SessionError):m.plan_restore(self.s,doc(),{},'new-boot')
 def test_replacement_agent_is_not_overwritten(self):
  with self.assertRaises(m.SessionError):m.plan_restore(self.s,doc(),{'t1':{'state':'live','status':{'app':'pi'}}},'new-boot')
 def test_busy_recovery_terminal_is_not_overwritten(self):
  with self.assertRaises(m.SessionError):m.plan_restore(self.s,doc(),{'t1':{'state':'live','foreground':{'argv':['npm','test']}}},'new-boot')
 def test_recovery_shell_can_be_replaced(self):
  live={'t1':{'state':'live','pid':20,'argv':['/bin/zsh','-l'],'foreground':{'argv':['/bin/zsh','-l']}}}
  self.assertEqual(m.plan_restore(self.s,doc(),live,'new-boot')[0]['action'],'resume')
 def test_missing_directory_blocks_restore(self):
  self.s['terminals']['t1']['cwd']=self.temp.name+'/missing'
  with self.assertRaises(m.SessionError):m.plan_restore(self.s,doc(),{},'new-boot')
 def test_changed_pane_layout_blocks_restore(self):
  with self.assertRaises(m.SessionError):m.plan_restore(self.s,doc('t2'),{},'new-boot')
 def test_merge_preserves_extra_work_and_split_properties(self):
  current=doc();extra=copy.deepcopy(current['spaces'][0]);extra['id']='s2';current['spaces'].append(extra)
  self.s['document']['spaces'][0]['tabs'][0]['root']['weights']=[0.4,0.6]
  result=m.merge_document(self.s['document'],current,{'t1':'t9'})
  self.assertEqual(len(result['spaces']),2);self.assertEqual(next(m.leaves(result))[3],'t9')
  self.assertEqual(result['spaces'][0]['tabs'][0]['root']['weights'],[0.4,0.6])
  self.assertEqual(next(m.leaves(self.s['document']))[3],'t1')
 def test_atomic_snapshot_permissions(self):
  p=pathlib.Path(self.temp.name)/'private.json';m.atomic_json(p,{'test':True})
  self.assertEqual(p.stat().st_mode & 0o777,0o600);self.assertEqual(m.read_json(p),{'test':True})
 def test_stale_manual_binding_is_rejected(self):
  t={'address':'t1','status':{'app':'codex'},'foreground':{'pid':90,'argv':['codex']}}
  binding={'t1':{'agent':'codex','pid':90,'birth':'old','ref':self.s['terminals']['t1']['ref']}}
  with patch.object(m,'process_birth',return_value='new'),patch.object(m,'read_json',return_value={}),patch.object(m.shutil,'which',return_value=None):
   self.assertIsNone(m.resolve_ref(t,binding)[1])
 def test_launch_resume_argument_is_not_assumed_current(self):
  t={'address':'t1','status':{'app':'codex'},'foreground':{'pid':90,'argv':['codex','resume',self.s['terminals']['t1']['ref']]}}
  with patch.object(m,'process_birth',return_value='new'),patch.object(m,'read_json',return_value={}),patch.object(m.shutil,'which',return_value=None):
   self.assertIsNone(m.resolve_ref(t,{})[1])
 def test_pi_ref_must_be_real_session(self):
  p=pathlib.Path(self.temp.name)/'pi.jsonl';p.write_text('{"type":"session","id":"example"}\n')
  self.assertEqual(m.validate_ref('pi',str(p)),str(p))
  p.write_text('{"type":"message"}\n')
  with self.assertRaises(m.SessionError):m.validate_ref('pi',str(p))
 def test_preview_has_no_mutations(self):
  calls=[]
  def rpc(method,params):
   calls.append(method)
   if method=='doc.get':return {'version':1,'value':doc()}
   raise m.SessionError('not found')
  with patch.object(m,'STATE',pathlib.Path(self.temp.name)),patch.object(m,'rpc',rpc),patch.object(m,'boot_id',return_value='new-boot'):
   result=m.restore(self.s)
  self.assertEqual(result['actions'][0]['action'],'resume');self.assertTrue(all(x in ['doc.get','terminal.get'] for x in calls))
 def test_failed_document_commit_rolls_back_only_created_terminal(self):
  calls=[]
  def rpc(method,params):
   calls.append((method,params))
   if method=='doc.get':return {'version':1,'value':doc()}
   if method=='terminal.get':raise m.SessionError('not found')
   if method=='terminal.run':return {'address':'t9','pid':90,'created_ms':9}
   if method=='doc.set':raise m.SessionError('version conflict')
   if method=='terminal.kill':return {}
   raise AssertionError(method)
  with patch.object(m,'STATE',pathlib.Path(self.temp.name)),patch.object(m,'rpc',rpc),patch.object(m,'boot_id',return_value='new-boot'),patch.object(m.shutil,'which',return_value='/bin/codex'):
   with self.assertRaises(m.SessionError):m.restore(self.s,True)
  killed=[p['terminal'] for name,p in calls if name=='terminal.kill'];self.assertEqual(killed,['t9'])
 def test_successful_restore_and_repeated_restore_are_idempotent(self):
  calls=[]
  def rpc(method,params):
   calls.append((method,params))
   if method=='doc.get':return {'version':1,'value':doc()}
   if method=='terminal.get':raise m.SessionError('not found')
   if method=='terminal.run':return {'address':'t9','pid':90,'created_ms':9}
   if method in ['doc.set','terminal.label']:return {}
   raise AssertionError(method)
  with patch.object(m,'STATE',pathlib.Path(self.temp.name)),patch.object(m,'rpc',rpc),patch.object(m,'boot_id',return_value='new-boot'),patch.object(m.shutil,'which',return_value='/bin/codex'):
   m.restore(self.s,True);count=len(calls);m.restore(self.s,True);self.assertEqual(len(calls),count)
  latest=m.read_json(pathlib.Path(self.temp.name)/'latest.json');self.assertIn('t9',latest['terminals']);self.assertEqual(next(m.leaves(latest['document']))[3],'t9')

if __name__=='__main__':unittest.main()
