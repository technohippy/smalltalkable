require 'test/unit'
require 'smalltalkable'

class TestSmalltalkable < Test::Unit::TestCase
  def test_define_class
    assert_raise(NameError) do Counter end
    Object.subclass :Counter,
      instanceVariableNames: 'counterValue',
      classVariableNames: 'instanceCount',
      poolDictionaries: '',
      category: 'Category-Name'
    assert_nothing_raised do Counter end
    assert_equal Class, Counter.class
    assert_equal [:@counterValue, :@category], Counter.instance_variables
    assert_equal [:@@instanceCount], Counter.class_variables
    assert_equal 'Category-Name', Counter.category
  end

  def test_define_method
    Counter.compile '
      initialize value
        @counterValue = value'
    Counter.compile '
      counterValue
        @counterValue'
    Counter.compile '
      setCounterValue newValue
        @counterValue = newValue'
    Counter.compile '
      method a1, arg2:a2, arg3:a3
        [a1, a2, a3]'

    counter = Counter.new 0
    assert_equal 0, counter.counterValue
    counter.setCounterValue 1
    assert_equal 1, counter.counterValue
    assert_equal [1, 2, 3], counter.method(1, arg2:2, arg3:3)
    assert_equal [1, 2, 3], counter.method(1, arg3:3, arg2:2)
    assert_equal [1, 2, nil], counter.method(1, arg2:2)
    assert_equal [1, nil, 3], counter.method(1, arg3:3)
  end

  def test_define_class_method
    Counter.class.compile '
      classMethod
        "classMethod"'
    assert_equal 'classMethod', Counter.classMethod
  end

  def test_if
    assert_equal :true, (1 == 1).if_true(->{:true}, if_false:->{:false})
    assert_equal :false, (0 == 1).if_true(->{:true}, if_false:->{:false})
    assert_equal :true, (1 == 1).if_true(->{:true})
    assert_nil (0 == 1).if_true(->{:true})

    assert_equal :true, (1 == 1).if_false(->{:false}, if_true:->{:true})
    assert_equal :false, (0 == 1).if_false(->{:false}, if_true:->{:true})
    assert_nil (1 == 1).if_false(->{:false})
    assert_equal :false, (0 == 1).if_false(->{:false})
  end

  def test_if_nil
    assert_equal :true, nil.if_nil(->{:true}, if_not_nil:->{:false})
    assert_equal :false, 1.if_nil(->{:true}, if_not_nil:->{:false})
    assert_equal :true, nil.if_nil(->{:true})
    assert_nil 1.if_nil(->{:true})
  end

  def test_while
    ret = ''
    c = 9
    ->{0 < c}.while_true ->{
      ret += c.to_s
      c -= 1
    }
    assert_equal '987654321', ret
  end
end
