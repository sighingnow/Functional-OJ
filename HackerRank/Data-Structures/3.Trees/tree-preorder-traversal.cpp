void preOrder(node *root) {
    printf("%d ", root->data);
    if (root->left) {
        preOrder(root->left);
    }
    if (root->right) {
        preOrder(root->right);
    }
}
