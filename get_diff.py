import difflib
import json

from tqdm import tqdm

if __name__ == '__main__':
    with open("OUTPUT/clean_history.json", 'r', encoding='utf-8') as history:
        lines = history.readlines()

    for line in lines:
        item = json.loads(line)
        revisions = item.get('revisions')

        if revisions[0][0].startswith('2020') and revisions[-1][0].startswith('2023'):
            old = '\n'.join(revisions[0][1]).splitlines()
            new = '\n'.join(revisions[-1][1]).splitlines()

            print(old)
            print(new)

            d = difflib.Differ()
            print()
            diff = list(d.compare(old, new))
            for item in diff:
                if item.startswith("+"):
                    print("----" + item[2:])
                    assert item[2:] in new
            # print(len(list(d.compare(old, new))))
            # print(len(new))
            # print(len(old))
            # print("".join(list(d.compare(old, new))))

            # compare = difflib.SequenceMatcher()
            # compare_result = compare.(old, new)
            # with open("diff.html", 'w', encoding='utf-8') as fp:
            #     fp.writelines(compare_result)

            # print("".join(list(d.compare(old, new))))
