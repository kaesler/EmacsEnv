^^c++-^^/usr1/dingus/weiner/projs/oobr/samples-c++/templates/OOBR
NO-ROOT
  ConstPtrList<T>
    +ConstPtrList^^ConstPtrList<T>@+ ConstPtrList@ConstPtrList(const ConstPtrList<T> &) : List<__AnyConstPtr>() {
    +ConstPtrList^^ConstPtrList<T>@+ ConstPtrList@ConstPtrList(long size) : List<__AnyConstPtr>(size) {
    +ConstPtrList^^ConstPtrList<T>@+ ConstPtrList@ConstPtrList() : List<__AnyConstPtr>() {
    -append^^ConstPtrList<T>@- append@void append(const ConstPtrList<T>& list) {
    -append^^ConstPtrList<T>@- append@void append(const T* item) {
    -count^^ConstPtrList<T>@- count@long count() const {
    -insert^^ConstPtrList<T>@- insert@void insert(long index, const ConstPtrList<T>& list) {
    -insert^^ConstPtrList<T>@- insert@void insert(long index, const T* item) {
    -item^^ConstPtrList<T>@- item@const T* item(long index) const {
    -operator =^^ConstPtrList<T>@- operator =@ConstPtrList<T>& operator = (const ConstPtrList<T>& l) {
    -prepend^^ConstPtrList<T>@- prepend@void prepend(const ConstPtrList<T>& list) {
    -prepend^^ConstPtrList<T>@- prepend@void prepend(const T* item) {
    -remove^^ConstPtrList<T>@- remove@void remove(long index) {
    -remove_all^^ConstPtrList<T>@- remove_all@void remove_all() {
  ConstPtrListItr<T>
    +ConstPtrListItr^^ConstPtrListItr<T>@+ ConstPtrListItr@ConstPtrListItr(const ConstPtrList<T>& l) : ListItr<__AnyConstPtr>(l) {
    -cur^^ConstPtrListItr<T>@- cur@const T* cur() const {
    -more^^ConstPtrListItr<T>@- more@boolean more() const {
    -next^^ConstPtrListItr<T>@- next@void next() {
  ConstPtrListUpdater<T>
    +ConstPtrListUpdater^^ConstPtrListUpdater<T>@+ ConstPtrListUpdater@ConstPtrListUpdater(ConstPtrList<T>& l) : ListUpdater<__AnyConstPtr>(l) {
    -cur^^ConstPtrListUpdater<T>@- cur@const T* cur() const {
    -more^^ConstPtrListUpdater<T>@- more@boolean more() const {
    -next^^ConstPtrListUpdater<T>@- next@void next() {
    -remove_cur^^ConstPtrListUpdater<T>@- remove_cur@void remove_cur() {
  List<T>
    +List^^List<T>@+ List@template <class T> List<T>::List(const List<T> &l) {
    +List^^List<T>@+ List@template <class T> List<T>::List() {
    +List^^List<T>@+ List@template <class T> List<T>::List(long size) {
    -append^^List<T>@- append@void append(const List<T>& list) {
    -append^^List<T>@- append@void append(const T& item) {
    -copy^^List<T>@- copy@template <class T> void List<T>::copy(const List<T> &l) {
    -count^^List<T>@- count@long count() const {
    -insert^^List<T>@- insert@template <class T> void List<T>::insert(long index, const List<T>& list) {
    -insert^^List<T>@- insert@template <class T> void List<T>::insert(long index, const T& item) {
    -item^^List<T>@- item@T item(long index) const {
    -item_ref^^List<T>@- item_ref@T& item_ref(long index) const{
    -operator =^^List<T>@- operator =@template <class T> List<T>& List<T>::operator = (const List<T> &l) {
    -prepend^^List<T>@- prepend@void prepend(const List<T>& list) {
    -prepend^^List<T>@- prepend@void prepend(const T& item) {
    -remove^^List<T>@- remove@template <class T> void List<T>::remove(long index) {
    -remove_all^^List<T>@- remove_all@template <class T> void List<T>::remove_all() {
    +~List^^List<T>@+ ~List@template <class T> List<T>::~List() {
  ListItr<T>
    +ListItr^^ListItr<T>@+ ListItr@template <class T> ListItr<T>::ListItr(const List<T>& list) {
    -cur^^ListItr<T>@- cur@T cur() const {
    -cur_ref^^ListItr<T>@- cur_ref@T& cur_ref() const {
    -more^^ListItr<T>@- more@boolean more() const {
    -next^^ListItr<T>@- next@void next() {
  ListUpdater<T>
    +ListUpdater^^ListUpdater<T>@+ ListUpdater@template <class T> ListUpdater<T>::ListUpdater(List<T>& list) {
    -cur^^ListUpdater<T>@- cur@T cur() const {
    -cur_ref^^ListUpdater<T>@- cur_ref@T& cur_ref() const {
    -more^^ListUpdater<T>@- more@boolean more() const {
    -next^^ListUpdater<T>@- next@void next() {
    -remove_cur^^ListUpdater<T>@- remove_cur@void remove_cur() {
  PtrList<T>
    +PtrList^^PtrList<T>@+ PtrList@PtrList(const PtrList<T> &) : List<__AnyPtr>() {
    +PtrList^^PtrList<T>@+ PtrList@PtrList(long size) : List<__AnyPtr>(size) {
    +PtrList^^PtrList<T>@+ PtrList@PtrList() : List<__AnyPtr>() {
    -append^^PtrList<T>@- append@void append(const PtrList<T>& list) {
    -append^^PtrList<T>@- append@void append(T* item) {
    -count^^PtrList<T>@- count@long count() const {
    -insert^^PtrList<T>@- insert@void insert(long index, const PtrList<T>& list) {
    -insert^^PtrList<T>@- insert@void insert(long index, T* item) {
    -item^^PtrList<T>@- item@T* item(long index) const {
    -item_ref^^PtrList<T>@- item_ref@T*& item_ref(long index) const {
    -operator =^^PtrList<T>@- operator =@PtrList<T>& operator = (const PtrList<T>& l) {
    -prepend^^PtrList<T>@- prepend@void prepend(const PtrList<T>& list) {
    -prepend^^PtrList<T>@- prepend@void prepend(T* item) {
    -remove^^PtrList<T>@- remove@void remove(long index) {
    -remove_all^^PtrList<T>@- remove_all@void remove_all() {
  PtrListItr<T>
    +PtrListItr^^PtrListItr<T>@+ PtrListItr@PtrListItr(const PtrList<T>& l) : ListItr<__AnyPtr>(l) {
    -cur^^PtrListItr<T>@- cur@T* cur() const {
    -more^^PtrListItr<T>@- more@boolean more() const {
    -next^^PtrListItr<T>@- next@void next() {
  PtrListUpdater<T>
    +PtrListUpdater^^PtrListUpdater<T>@+ PtrListUpdater@PtrListUpdater(PtrList<T>& l) : ListUpdater<__AnyPtr>(l) {
    -cur^^PtrListUpdater<T>@- cur@T* cur() const {
    -more^^PtrListUpdater<T>@- more@boolean more() const {
    -next^^PtrListUpdater<T>@- next@void next() {
    -remove_cur^^PtrListUpdater<T>@- remove_cur@void remove_cur() {
