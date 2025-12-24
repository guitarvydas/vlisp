import sys
sys.path.insert(0, './pbp/kernel')
import kernel0d as zd

[palette, env] = zd.initialize_from_files (sys.argv[1], sys.argv[4:])
top = zd.start (arg=sys.argv[2], part_name=sys.argv[3], palette=palette, env=env)
