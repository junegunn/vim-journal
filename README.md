# vim-journal

![](https://cloud.githubusercontent.com/assets/700826/7340304/6763bb9a-ecc5-11e4-998b-7dd9b0b12195.png)

# Installation

`Plug 'junegunn/vim-journal'`

# Usage

There are 4 ways to enable the syntax on a file.

1. Add: `/* vim: set filetype=journal! */` at the top of any file.
2. Create a `.txt` file in a folder called `notes` or `journal.d` and it will automatically understand the filetype.
3. Run the command: `:set filetype=journal`. 
4. Alternatively, you can map the previous command in your `.vimrc` to something like: `nmap <leader>L :set filetype=journal<cr>`
