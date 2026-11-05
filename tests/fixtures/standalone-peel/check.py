import json, sys
d = json.load(sys.stdin)
sys.exit(0 if eval(sys.argv[1], {"d": d}) else 1)
