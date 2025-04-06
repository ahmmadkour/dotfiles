vim.keymap.set("n", "<leader>gs", vim.cmd.Git);
vim.keymap.set('n', '<Leader>gb', ':GBrowse<CR>', { desc = "Git: Browse current line", noremap = true, silent = true })
vim.keymap.set('v', '<Leader>gb', ':GBrowse<CR>', { desc = "Git: Browse visual selection", noremap = true, silent = true })
