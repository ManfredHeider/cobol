      *****************************************************************
      *    COPY-MEMBER: DRKI1974                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI1974                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  20.11.2006/12:21:22
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI1974
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1974.
           05  PI-DRKI1974-KEY.


             10 PI-DRKI1974-VEGGEFAHRCODE   PIC S9(4) COMP.
             10 PI-DRKI1974-VEGVERSTARLFDNR   PIC S9(4) COMP.
             10 PI-DRKI1974-VEGVERTRAGSID   PIC X(13).
             
      *
       01  PO-DRKI1974.
           05  PO-DRKI1974-RC                 PIC  X(003).
               88 PO-DRKI1974-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1974-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRKI1974-DATEN.


             10 PO-DRKI1974-VEGKZSBNL   PIC X(1).
             10 PO-DRKI1974-VEGSBMAX   PIC S9(5)V COMP-3.
             10 PO-DRKI1974-VEGSBMIN   PIC S9(5)V COMP-3.
             10 PO-DRKI1974-VEGSBSCHADEN   PIC S9(2)V COMP-3.
             10 PO-DRKI1974-VEGSBVSUMME   PIC S9(2)V COMP-3.
             10 PO-DRKI1974-VEGSELBSTBETEIL   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI1974-VEGSELBSTBETWKZ   PIC X(01).
             10 PO-DRKI1974-VEGTARIFZONE   PIC S9(4) COMP.
             

      *
      ******* ENDE COPY-MEMBER **************************************
