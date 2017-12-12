node *insert(node * root, int value) {
    if (root == NULL) {
        root = (node *)malloc(sizeof(node));
        root->data = value;
        root->left = root->right = NULL;
    }
    else {
        if (value < root->data) {
            root->left = insert(root->left, value);
        }
        else {
            root->right = insert(root->right, value);
        }
    }

    return root;
}
