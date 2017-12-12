void postOrder(node *root) {
    if (root->left) {
        postOrder(root->left);
    }
    if (root->right) {
        postOrder(root->right);
    }
    printf("%d ", root->data);
}
