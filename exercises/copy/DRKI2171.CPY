      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.09.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2171.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2171.CPv  $
      *
      *   Rev 2.1   17 Oct 2007 13:13:48   t13ex27
      *AKL00665: Allfinanz; Firmenkunden AML-Standard-OZPs
      *
      *   Rev 2.0   28 Sep 2007 18:00:22   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2171                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2171                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  17.10.2007/12:09:40
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
      *    FÜR DIE FUNKTION DRKI2171
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2171.
           05  PI-DRKI2171-KEY.


             10 PI-DRKI2171-VSBRISIGRUPPEID   PIC S9(4) COMP.
             10 PI-DRKI2171-VSBSELBSTBETID   PIC S9(4) COMP.
             10 PI-DRKI2171-VSBVERTRAGSID   PIC X(13).
             10 PI-DRKI2171-VSBVETLFDNR   PIC S9(4) COMP.
             

           05  PI-DRKI2171-DATEN.


             10 PI-DRKI2171-VSBBEZEICHNUNG   PIC X(50).
             10 PI-DRKI2171-VSBEINZVTGHOSTID   PIC X(15).
             10 PI-DRKI2171-VSBHOECHSTBEITR   PIC S9(5)V9(2) COMP-3.
             10 PI-DRKI2171-VSBMINDESTBEITR   PIC S9(5)V9(2) COMP-3.
             10 PI-DRKI2171-VSBSELBSTBTEXT   PIC X(30).
             10 PI-DRKI2171-VSBWERT   PIC S9(8)V9(2) COMP-3.
             10 PI-DRKI2171-VSBWERTPROZENT   PIC S9(4) COMP.
             

      *
       01  PO-DRKI2171.
           05  PO-DRKI2171-RC                 PIC  X(003).
               88 PO-DRKI2171-RC-OK                        VALUE 'OK '.
               88 PO-DRKI2171-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
