import bz2
import json
import os
import xml.dom.minidom as minidom
from tqdm import tqdm


def decode_open(filename, mode='rt', encoding='utf-8'):
    """
    Open a file, decode and decompress, depending on extension `gz`, or 'bz2`.
    :param filename: the file to open.
    """
    ext = os.path.splitext(filename)[1]
    if ext == '.gz':
        import gzip
        return gzip.open(filename, mode, encoding=encoding)
    elif ext == '.bz2':
        return bz2.open(filename, mode=mode, encoding=encoding)
    else:
        return open(filename, mode, encoding=encoding)


output = open('OUTPUT/history.json', 'w', encoding='utf-8')
dom = minidom.parse('DATA/zhwiki-20230201-pages-meta-history6.xml-p8141535p8266476')
root = dom.documentElement
# print(root.childNodes)
names = root.getElementsByTagName('page')
for name in tqdm(names):
    # 它的第一个子节点是一个textnode，存取的是真正的节点值
    # print(name.childNodes)
    title = name.getElementsByTagName('title')
    title_text = title[0].childNodes[0].data

    id = name.getElementsByTagName('id')
    id_text = id[0].childNodes[0].data

    revisions_list = []
    revisions = name.getElementsByTagName('revision')
    for revision in revisions:
        timestamp = revision.getElementsByTagName('timestamp')
        text = revision.getElementsByTagName('text')

        timestamp_text = 'None'
        if len(timestamp[0].childNodes) != 0:
            timestamp_text = timestamp[0].childNodes[0].data

        text_text = 'None'
        if len(text[0].childNodes) != 0:
            text_text = text[0].childNodes[0].data

        revisions_list.append((timestamp_text, text_text))

    json_item = {'id': id_text, 'title': title_text, 'revisions': revisions_list}
    output.write(json.dumps(json_item, ensure_ascii=False) + '\n')
