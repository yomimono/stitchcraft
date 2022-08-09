# webapp

## sessions

* bio/profile
* privacy/permissions
* favorite tracking

## data

## interface

* PDF download
	* in the beginning, this can just be requests to /patterns/id with content-type `application/pdf`
* JS-less viewer
* create patterns
	* start with simple text, like on command line
	* text box, font chooser
		* need to get the fonts into the pgsql thing for that
		* textstitch has a schema for finding them, fontreader/src/font2postgres for writing them
		* does the db on legitcreds have them saved? not the full set, we just have a few, but that's enough to proof-of-concept the interface
* richer viewer/editor (composition)
	* left side: active thing
	* right side: pattern argument to any operation
	* operation selector
	* (far?) right side: operation history viewer
		* awkwardly, needs to be kept separately from the saved pattern
			* oh no this way goes badly reinventing smallltalk

# other stuff

## payments

* sell thing
* get money

## inventory management

### incoming (suppliers)
### outgoing (orders)

## fulfillment

---

# done

## 9th Aug
* fix a bug in estimator totals (it didn't include substrate fabric cost)
* add fabric estimate to display.ml output
* ran sqlmap against the tag search ("does not seem to be injectable", yay)
* normalize to "textstitch", not "textsay"
* pull the canvas stuff out of `display` so we can easily use it in an editor
