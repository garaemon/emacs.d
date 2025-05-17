import re

def sort_use_package_blocks(file_path):
    try:
        with open(file_path, 'r', encoding='utf-8') as f:
            content = f.read()
    except FileNotFoundError:
        # print(f"Error: File not found at {file_path}")
        return
    except Exception as e:
        # print(f"Error reading file {file_path}: {e}")
        return

    package_blocks_meta = [] 
    original_blocks_info = [] 
    current_pos = 0

    while current_pos < len(content):
        match = re.search(r'(use-package\s+([A-Za-z0-9_-]+[A-Za-z0-9_/-]*))\b', content[current_pos:])
        if not match:
            break

        block_declaration_start_rel = match.start()
        block_declaration_start_abs = current_pos + block_declaration_start_rel
        package_name = match.group(2)
        s_expr_start = block_declaration_start_abs

        line_start_for_comment_check = content.rfind('\n', 0, s_expr_start) + 1
        line_end_for_comment_check = content.find('\n', s_expr_start)
        if line_end_for_comment_check == -1:
            line_end_for_comment_check = len(content)
        current_line_text = content[line_start_for_comment_check:line_end_for_comment_check]
        is_commented = current_line_text.strip().startswith(';;')

        open_parens = 0
        idx = s_expr_start
        found_end = False
        s_expr_end = -1

        while idx < len(content):
            char = content[idx]
            if char == ';':
                if (idx == 0 or content[idx-1] == '\n' or content[idx-1] == ' ' or content[idx-1] == '\t'):
                    if not (idx + 2 < len(content) and content[idx+1] == ';' and content[idx+2] == ';'):
                        comment_end = content.find('\n', idx)
                        if comment_end == -1:
                            idx = len(content)
                        else:
                            idx = comment_end
                        continue
            elif char == '"':
                str_idx = idx + 1
                while str_idx < len(content):
                    if content[str_idx] == '\\':
                        str_idx += 1
                        if str_idx >= len(content): break
                    elif content[str_idx] == '"':
                        break
                    str_idx += 1
                idx = str_idx
            elif char == '(':
                open_parens += 1
            elif char == ')':
                open_parens -= 1
                if open_parens == 0:
                    s_expr_end = idx + 1
                    found_end = True
                    break
            idx += 1
        
        if found_end:
            block_text = content[s_expr_start:s_expr_end]
            block_info = {
                "name": package_name,
                "block_text": block_text,
                "start_offset": s_expr_start,
                "end_offset": s_expr_end,
                "is_commented": is_commented
            }
            original_blocks_info.append(block_info)
            if not is_commented:
                package_blocks_meta.append(block_info)
            current_pos = s_expr_end
        else:
            current_pos = block_declaration_start_abs + len(match.group(1))
            if current_pos >= len(content):
                break
    
    if not package_blocks_meta:
        # print("No non-commented use-package blocks found to sort. File will not be changed.")
        return

    package_blocks_meta.sort(key=lambda x: x['name'])

    new_content_parts = []
    last_processed_end = 0
    sorted_block_insert_idx = 0
    original_blocks_info.sort(key=lambda x: x['start_offset'])

    for block_info in original_blocks_info:
        new_content_parts.append(content[last_processed_end:block_info['start_offset']])
        if block_info['is_commented']:
            new_content_parts.append(block_info['block_text'])
        else:
            if sorted_block_insert_idx < len(package_blocks_meta):
                new_content_parts.append(package_blocks_meta[sorted_block_insert_idx]['block_text'])
                sorted_block_insert_idx += 1
            else:
                new_content_parts.append(block_info['block_text'])
        last_processed_end = block_info['end_offset']
    
    new_content_parts.append(content[last_processed_end:])
    final_content = "".join(new_content_parts)

    try:
        with open(file_path, 'w', encoding='utf-8') as f:
            f.write(final_content)
        # print(f"Successfully sorted use-package blocks in {file_path}")
    except Exception as e:
        # print(f"Error writing file {file_path}: {e}")
        pass

if __name__ == '__main__':
    init_el_path = "init.el"
    sort_use_package_blocks(init_el_path)
