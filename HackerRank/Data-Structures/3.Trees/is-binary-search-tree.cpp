/* Hidden stub code will pass a root argument to the function below. Complete the function to solve the challenge. Hint: you may want to write one or more helper functions.

The Node struct is defined as follows:

*/
    struct Node {
      int data;
      Node* left;
      Node* right;
   };


struct _tuple {
    bool _0;
    int _1;
    int _2;
    _tuple(bool _0, int _1, int _2) {
        this->_0 = _0;
        this->_1 = _1;
        this->_2 = _2;
    }
};

// @root must not be NULL
// @maxmin true: max, false: min.
_tuple traverseMaxMin(Node *root) {
    _tuple validLeft = _tuple(true, root->data, root->data - 1),
           validRight = _tuple(true, root->data + 1, root->data);
    if (!root->left && !root->right) {
        return _tuple(true, root->data, root->data);
    }
    if (root->left) {
        validLeft = traverseMaxMin(root->left);
    }
    if (root->right) {
        validRight = traverseMaxMin(root->right);
    }
    if (validLeft._0 && validRight._1
                     && validLeft._2 < root->data
                     && validRight._1 > root->data) {
        return _tuple(true, validLeft._1, validRight._2);
    } else {
        return _tuple(false, root->data, root->data);
    }
}


bool checkBST(Node* root) {
    if (root) {
        _tuple validLeft = _tuple(true, root->data, root->data - 1),
               validRight = _tuple(true, root->data + 1, root->data);
        if (root->left) {
            validLeft = traverseMaxMin(root->left);
        }
        if (root->right) {
            validRight = traverseMaxMin(root->right);
        }
        return validLeft._0 && validRight._0
                            && validLeft._2 < root->data
                            && validRight._1 > root->data;
    } else {
        return true;
    }
}

