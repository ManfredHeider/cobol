      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.09.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2173.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2173.CPv  $
      *
      *   Rev 2.1   17 Oct 2007 13:14:12   t13ex27
      *AKL00665: Allfinanz; Firmenkunden AML-Standard-OZPs
      *
      *   Rev 2.0   28 Sep 2007 18:06:38   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2173                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2173                    *
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
      *    FÜR DIE FUNKTION DRKI2173
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2173.
           05  PI-DRKI2173-KEY.


             10 PI-DRKI2173-VSBRISIGRUPPEID   PIC S9(4) COMP.
             10 PI-DRKI2173-VSBSELBSTBETID   PIC S9(4) COMP.
             10 PI-DRKI2173-VSBVERTRAGSID   PIC X(13).
             10 PI-DRKI2173-VSBVETLFDNR   PIC S9(4) COMP.
             
      *
       01  PO-DRKI2173.
           05  PO-DRKI2173-RC                 PIC  X(003).
               88 PO-DRKI2173-RC-OK                        VALUE 'OK '.
               88 PO-DRKI2173-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRKI2173-DATEN.


             10 PO-DRKI2173-LETZTEAENDERUNG   PIC X(26).
             10 PO-DRKI2173-VSBBEZEICHNUNG   PIC X(50).
             10 PO-DRKI2173-VSBEINZVTGHOSTID   PIC X(15).
             10 PO-DRKI2173-VSBHOECHSTBEITR   PIC S9(5)V9(2) COMP-3.
             10 PO-DRKI2173-VSBMINDESTBEITR   PIC S9(5)V9(2) COMP-3.
             10 PO-DRKI2173-VSBSELBSTBTEXT   PIC X(30).
             10 PO-DRKI2173-VSBWERT   PIC S9(8)V9(2) COMP-3.
             10 PO-DRKI2173-VSBWERTPROZENT   PIC S9(4) COMP.
             

      *
      ******* ENDE COPY-MEMBER **************************************
