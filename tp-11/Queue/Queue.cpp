#include <iostream>
#include "Queue.h"
using namespace std;

struct NodeQ
{
    int value;
    NodeQ *next;
};

struct QueueSt
{
    int size;
    NodeQ *first;
    NodeQ *last;
};
typedef QueueSt *Queue;

Queue emptyQ()
{
    QueueSt *q = new QueueSt;
    q->size = 0;
    q->first = NULL;
    q->last = NULL;
    return q;
}

bool isEmptyQ(Queue q)
{
    return q->size == 0;
}

int firstQ(Queue q)
{
    if (q->size == 0)
    {
        throw invalid_argument("Error using firstQ: Is empty Queue");
    }
    return q->first->value;
}

void Enqueue(int x, Queue q)
{
    NodeQ *node = new NodeQ;
    node->value = x;
    node->next = NULL;
    if (q->first == NULL)
    {
        q->first = node;
    }
    else
    {
        q->last->next = node;
    }
    q->last = node;
    q->size++;
}

void Dequeue(Queue q)
{
    if (q->first == NULL)
    {
        throw invalid_argument("Error using Dequeue: Is empty Queue");
    }
    NodeQ *temp = q->first->next;
    delete q->first;
    q->first = temp;
    if (q->first == NULL)
    {
        q->last = NULL;
    }
    q->size--;
}

int lengthQ(Queue q)
{
    return q->size;
}

void MergeQ(Queue q1, Queue q2)
{
    if (q1->size == 0)
    {
        q1->first = q2->first;
        q1->last = q2->last;
    }
    else if (q2->size != 0)
    {
        q1->last->next = q2->first;
        q1->last = q2->last;
    }
    q1->size += q2->size;
    delete q2;
}

void DestroyQ(Queue q)
{
    NodeSet *temp = q->first;
    while (q->first != NULL)
    {
        q->first = temp->next;
        delete temp;
        temp = q->first;
    }
    delete q;
}