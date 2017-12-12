struct node
{
    int data;
    node* left;
    node* right;
};

void traverseLeft(node *root) {
    if (root->left) {
        traverseLeft(root->left);
        printf("%d ", root->left->data);
    }
}

void traverseRight(node *root) {
    if (root->right) {
        printf("%d ", root->right->data);
        traverseRight(root->right);
    }
}

void topView(node * root) {
    traverseLeft(root);
    printf("%d ", root->data);
    traverseRight(root);
}

