void inOrder(node *root) {
    if (root->left) {
        inOrder(root->left);
    }
    printf("%d ", root->data);
    if (root->right) {
        inOrder(root->right);
    }
}
