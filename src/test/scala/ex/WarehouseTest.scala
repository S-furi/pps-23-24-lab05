package ex

import org.junit.jupiter.api.{BeforeEach, Test}
import org.junit.jupiter.api.Assertions.{assertEquals, assertNotNull, assertTrue, assertFalse}
import util.Sequences.*
import util.Sequences.Sequence.*

class WarehouseTest:
  private val i = Item(code = 1, name = "test", "a", "b", "c")
  private var w = Warehouse()

  @BeforeEach def setUp(): Unit = {
    w = Warehouse()
    w.store(i)
  }

  @Test def testItemCreation(): Unit = {
    assertEquals(1, i.code)
    assertEquals("test", i.name)
    assertEquals(Sequence("a", "b", "c"), i.tags)
  }

  @Test def testWarehouseContains(): Unit = {
    assertFalse(w.contains(-1))
    assertTrue(w.contains(i.code))
  }

  @Test def testWarehouseCanStoreItems(): Unit = {
    val i2 = Item(code = 2, "test2", "foo")
    w.store(i2)
    assertTrue(w.contains(i.code))
    assertTrue(w.contains(i2.code))
  }

  @Test def testWarehouseCanSearchItem(): Unit = {
    val i2 = Item(code = 2, "test2", "a")
    w.store(i2)
    val items = w.searchItems("a")
    assertTrue(items.contains(i))
    assertTrue(items.contains(i2))
  }

  @Test def testWarehouseCanRetrieveItem(): Unit = {
    val item = w.retrieve(i.code)
    assertFalse(item.isEmpty)
    assertEquals(i, item.orElse(Item(code = -1, name = "error")))
  }

  @Test def testWarehouseCanRemoveItem(): Unit = {
    val i2 = Item(code = 2, "test2", "a")
    w.store(i2)
    w.remove(i2)
    assertTrue(w.retrieve(i2.code).isEmpty)
  }