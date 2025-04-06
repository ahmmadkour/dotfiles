-- vim config home dir
vim.keymap.set("n", "<leader>pv", vim.cmd.Ex)

-- Buffer
vim.keymap.set("n", "<C-p>", "<cmd>:bp<CR>");
vim.keymap.set("n", "<C-n>", "<cmd>:bn<CR>");

-- Move selected lines in visual mode
vim.keymap.set("v", "J", ":m '>+1<CR>gv=gv")
vim.keymap.set("v", "K", ":m '<-2<CR>gv=gv")

-- Allow your cusor to stay in the same place
vim.keymap.set("n", "J", "mzJ`z")

-- Allow cursor to be in the middle
vim.keymap.set("n", "<C-d>", "<C-d>zz")
vim.keymap.set("n", "<C-u>", "<C-u>zz")

-- Allow search cursor to be in the middle
vim.keymap.set("n", "n", "nzzzv")
vim.keymap.set("n", "N", "Nzzzv")

-- Copy paste without losing current paste buffer - copying to void register
vim.keymap.set("x", "<leader>p", [["_dP]])

-- copy to system clipboard
vim.keymap.set({"n", "v"}, "<leader>y", [["+y]])
vim.keymap.set("n", "<leader>Y", [["+Y]])

-- Deleting to void register
vim.keymap.set({"n", "v"}, "<leader>d", [["_d]])

-- Use C-c instead of Esc to exit insert mode
vim.keymap.set("i", "<C-c>", "<Esc>")

-- Ignore Q
vim.keymap.set("n", "Q", "<nop>")

-- Formatting
vim.keymap.set("n", "<leader>t", vim.lsp.buf.format)

-- Selected word quick Search/Replace
vim.keymap.set("n", "<leader>r", [[:%s/\<<C-r><C-w>\>/<C-r><C-w>/gI<Left><Left><Left>]])

-- Make a file executable
vim.keymap.set("n", "<leader>x", "<cmd>!chmod +x %<CR>", { silent = true })

-- Code navigation
vim.keymap.set("n", "<C-]>", vim.lsp.buf.definition)
vim.keymap.set("n", "gd", vim.lsp.buf.definition)
vim.keymap.set("n", "K", vim.lsp.buf.hover)
vim.keymap.set("n", "gD", vim.lsp.buf.implementation)
vim.keymap.set("n", "<C-k>", vim.lsp.buf.signature_help)
vim.keymap.set("n", "1gD", vim.lsp.buf.type_definition)
vim.keymap.set("n", "gr", vim.lsp.buf.references)
vim.keymap.set("n", "g0", vim.lsp.buf.document_symbol)
vim.keymap.set("n", "gW", vim.lsp.buf.workspace_symbol)
vim.keymap.set("n", "ga", vim.lsp.buf.code_action)

-- configuration reload
vim.keymap.set("n", "<leader><leader>", function()
    vim.cmd("so")
end)

