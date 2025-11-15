#include "hw7.h"

bst_sf* insert_bst_sf(matrix_sf *mat, bst_sf *root) {
    if (root == NULL) {
        root = malloc(sizeof(bst_sf));
        (*root).mat = mat;
        (*root).left_child = NULL;
        (*root).right_child = NULL;
    } else {
        if ((*mat).name < (*(*root).mat).name) {
            (*root).left_child = insert_bst_sf(mat, (*root).left_child);
        } else {
            (*root).right_child = insert_bst_sf(mat, (*root).right_child);
        }
    }
    return root;
}

matrix_sf* find_bst_sf(char name, bst_sf *root) {
    if (root == NULL) {
        return NULL;
    }
    if (name < (*(*root).mat).name) {
        return find_bst_sf(name, (*root).left_child);
    }
    if (name > (*(*root).mat).name) {
        return find_bst_sf(name, (*root).right_child);
    }
    return (*root).mat;
}

void free_bst_sf(bst_sf *root) {
    if (root == NULL) {
        return;
    }
    free_bst_sf((*root).left_child);
    free_bst_sf((*root).right_child);
    free((*root).mat);
    free(root);
}

matrix_sf* add_mats_sf(const matrix_sf *mat1, const matrix_sf *mat2) {
    if (((*mat1).num_rows == (*mat2).num_rows) && ((*mat1).num_cols == (*mat2).num_cols)) {
        int numRows = (*mat1).num_rows;
        int numCols = (*mat1).num_cols;
        matrix_sf *compMatrix = malloc(sizeof(matrix_sf)+numRows*numCols*sizeof(int));
        if (!compMatrix) return NULL;
        (*compMatrix).num_rows = numRows;
        (*compMatrix).num_cols = numCols;
        for (int x = 0; x < numRows; x++) {
            for (int y = 0; y < numCols; y++) {
                int matrixIndex = x * numCols + y;
                *((*compMatrix).values + matrixIndex) = *((*mat1).values + matrixIndex) + *((*mat2).values + matrixIndex);
            }
        }   
        return compMatrix;
    } else {
        return NULL;
    }
}

matrix_sf* mult_mats_sf(const matrix_sf *mat1, const matrix_sf *mat2) {
    if (((*mat1).num_cols == (*mat2).num_rows)) {
        int numRows = (*mat1).num_rows;
        int numCols = (*mat2).num_cols;
        matrix_sf *compMatrix = malloc(sizeof(matrix_sf)+numRows*numCols*sizeof(int));
        if (!compMatrix) return NULL;
        (*compMatrix).num_rows = numRows;
        (*compMatrix).num_cols = numCols;
        int sharedValue = (*mat1).num_cols;
        for (int x = 0; x < numRows; x++) {
            for (int y = 0; y < numCols; y++) {
                int sum = 0;
                for (int z = 0; z < sharedValue; z++) {
                    int rowMat1 = x * sharedValue + z;
                    int colMat2 = z * numCols + y;
                    sum += *((*mat1).values + rowMat1) * *((*mat2).values + colMat2);
                }
            int matrixIndex = x * numCols + y;
            *((*compMatrix).values + matrixIndex) = sum;
            }
        }   
        return compMatrix;
    } else {
        return NULL;
    }
}

matrix_sf* transpose_mat_sf(const matrix_sf *mat) {
    int numRows = (*mat).num_cols;
    int numCols = (*mat).num_rows;
    matrix_sf *swappedMatrix = malloc(sizeof(matrix_sf)+numRows*numCols*sizeof(int));
    if (!swappedMatrix) return NULL;
    (*swappedMatrix).num_rows = numRows;
    (*swappedMatrix).num_cols = numCols;
    for (int x = 0; x < numCols; x++) {
        for (int y = 0; y < numRows; y++) {
            int matrixIndex = x * numRows + y;
            int swappedMatrixIndex = y * numCols + x;
            *((*swappedMatrix).values + swappedMatrixIndex) = *((*mat).values + matrixIndex);
        }
    }
    return swappedMatrix;
}

matrix_sf* create_matrix_sf(char name, const char *expr) {
    const char *itr = expr;
    char *checkpoint = NULL;
    int numRows = 0, numCols = 0;
    while (*itr && !(*itr >= '0' && *itr <= '9')) {
        itr++;
    }
    numRows = strtol(itr, &checkpoint, 10);
    itr = checkpoint;
    while (*itr && !(*itr >= '0' && *itr <= '9')) {
        itr++;
    }
    numCols = strtol(itr, &checkpoint, 10);
    itr = checkpoint;
    matrix_sf *createdMatrix = malloc(sizeof(matrix_sf)+numRows*numCols*sizeof(int));
    if (!createdMatrix) return NULL;
    (*createdMatrix).name = name;
    (*createdMatrix).num_rows = numRows;
    (*createdMatrix).num_cols = numCols;
    while (*itr != '[') {
        itr++;
    }
    if (*itr == '[') {
        itr++;
    }
    for (int i = 0; i < numRows * numCols; i++) {
        while (*itr && !((*itr >= '0' && *itr <= '9') || *itr == '-' || *itr == '+')) {
            itr++;
        }
        int matrixValues = strtol(itr, &checkpoint, 10);
        (*createdMatrix).values[i] = matrixValues;
        itr = checkpoint;
    }
    return createdMatrix;
}

int findPrecedence(char oper) {
    if (oper == '\'') {
        return 3;
    } else if (oper == '*') {
        return 2;
    } else if (oper == '+') {
        return 1;
    }
    return 0;
}

char* infix2postfix_sf(char *infix) {
    char *post = malloc(MAX_LINE_LEN);
    char *stack = malloc(MAX_LINE_LEN);
    int openParen = 0;
    int infixItr = 0, postfixItr = 0, stackItr = 0;
    while (*(infix + infixItr) != '\0') {
        if (*(infix + infixItr) >= 'A' && *(infix + infixItr) <= 'Z') {
            *(post + postfixItr) = *(infix + infixItr);
            postfixItr++;
        } else if (*(infix + infixItr) == '+' || *(infix + infixItr) == '*' || *(infix + infixItr) == '\'') {
            while (stackItr > 0 && *(stack + stackItr - 1) != '(' &&
                findPrecedence(*(stack + stackItr - 1)) >= findPrecedence(*(infix + infixItr))) {
                stackItr--;
                *(post + postfixItr) = *(stack + stackItr);
                postfixItr++;
            }
            *(stack + stackItr) = *(infix + infixItr);
            stackItr++;
        } else if (*(infix + infixItr) == '(') {
            *(stack + stackItr) = *(infix + infixItr);
            stackItr++;
            openParen = 1;
        } else if (*(infix + infixItr) == ')') {
            while (stackItr > 0 && *(stack + stackItr - 1) != '(') {
                stackItr--;
                *(post + postfixItr) = *(stack + stackItr);
                postfixItr++;
            }
            if (stackItr > 0) {
                stackItr--;
            }
            openParen = 0;
        }
        infixItr++;
    }
    while (stackItr > 0) {
        stackItr--;
        *(post + postfixItr) = *(stack + stackItr);
        postfixItr++;
    }
    *(post + postfixItr) = '\0';
    free(stack);
    return post;
}

matrix_sf* evaluate_expr_sf(char name, char *expr, bst_sf *root) {
    char *postExpr = infix2postfix_sf(expr);
    matrix_sf **stack = malloc(MAX_LINE_LEN * sizeof(matrix_sf*));
    int postItr = 0, stackItr = 0;
    matrix_sf *firstOperand, *secondOperand, *result;
    while (*(postExpr + postItr) != '\0') {
        if (*(postExpr + postItr) >= 'A' && *(postExpr + postItr) <= 'Z') {
            *(stack + stackItr) = find_bst_sf(*(postExpr + postItr), root);
            stackItr++;
        } else if (*(postExpr + postItr) == '\'') {
            firstOperand = *(stack + stackItr - 1);
            result = transpose_mat_sf(firstOperand);
            *(stack + stackItr - 1) = result;
            if ((*firstOperand).name < 'A' || (*firstOperand).name > 'Z') {
                free(firstOperand);
            }
        } else if (*(postExpr + postItr) == '*') {
            stackItr--;
            firstOperand = *(stack + stackItr - 1);
            secondOperand = *(stack + stackItr);
            result = mult_mats_sf(firstOperand, secondOperand);
            *(stack + stackItr - 1) = result;
            if ((*firstOperand).name < 'A' || (*firstOperand).name > 'Z') {
                free(firstOperand);
            }
            if ((*secondOperand).name < 'A' || (*secondOperand).name > 'Z') {
                free(secondOperand);
            }
        } else if (*(postExpr + postItr) == '+') {
            stackItr--;
            firstOperand = *(stack + stackItr - 1);
            secondOperand = *(stack + stackItr);
            result = add_mats_sf(firstOperand, secondOperand);
            *(stack + stackItr - 1) = result;
            if ((*firstOperand).name < 'A' || (*firstOperand).name > 'Z') {
                free(firstOperand);
            }
            if ((*secondOperand).name < 'A' || (*secondOperand).name > 'Z') {
                free(secondOperand);
            }
        }
        postItr++;
    }
    (**(stack + stackItr - 1)).name = name;
    result = *(stack + stackItr - 1);
    free(stack);
    free(postExpr);
    return result;
}

matrix_sf *execute_script_sf(char *filename) {
    FILE *file = fopen(filename, "r");
    if (!file) return NULL;
    char *line = NULL;
    size_t bufferLength = MAX_LINE_LEN;
    bst_sf *binaryTree = NULL;
    matrix_sf *result = NULL;
    while (getline(&line, &bufferLength, file) != -1) {
        char *itr = line;
        char name = '\0';
        while (*itr && *itr != '=') {
            if (*itr >= 'A' && *itr <= 'Z') {
                name = *itr;
            }
            itr++;
        }
        if (!name) {
            continue;
        }
        itr++;
        char *scan = line;
        int checkMat = 0;
        while (*scan) {
            if (*scan == '[') {
                checkMat = 1;
            }
            scan++;
        }
        matrix_sf *mat = NULL;
        if (checkMat) {
            mat = create_matrix_sf(name, line);
        } else {
            mat = evaluate_expr_sf(name, itr, binaryTree);
        }
        if (mat) {
            binaryTree = insert_bst_sf(mat, binaryTree);
            result = mat;
        }
    }
    free(line);
    fclose(file);
    return result;
}

// This is a utility function used during testing. Feel free to adapt the code to implement some of
// the assignment. Feel equally free to ignore it.
matrix_sf *copy_matrix(unsigned int num_rows, unsigned int num_cols, int values[]) {
    matrix_sf *m = malloc(sizeof(matrix_sf)+num_rows*num_cols*sizeof(int));
    m->name = '?';
    m->num_rows = num_rows;
    m->num_cols = num_cols;
    memcpy(m->values, values, num_rows*num_cols*sizeof(int));
    return m;
}

// Don't touch this function. It's used by the testing framework.
// It's been left here in case it helps you debug and test your code.
void print_matrix_sf(matrix_sf *mat) {
    assert(mat != NULL);
    assert(mat->num_rows <= 1000);
    assert(mat->num_cols <= 1000);
    printf("%d %d ", mat->num_rows, mat->num_cols);
    for (unsigned int i = 0; i < mat->num_rows*mat->num_cols; i++) {
        printf("%d", mat->values[i]);
        if (i < mat->num_rows*mat->num_cols-1)
            printf(" ");
    }
    printf("\n");
}

