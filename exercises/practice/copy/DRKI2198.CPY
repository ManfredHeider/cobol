      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  29.09.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2198.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2198.CPv  $
      *
      *   Rev 2.2   Jun 21 2008 12:53:46   t13ex52
      *AKL00665: Allfinanz; Firmenkunden AMV
      *
      *   Rev 2.0   29 Sep 2007 12:56:40   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2198                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2198                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  28.05.2008/16:32:22
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
      *    FÜR DIE FUNKTION DRKI2198
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2198.
           05  PI-DRKI2198-KEY.


             10 PI-DRKI2198-VZAUNIVLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2198-VZAVERTRAGSID   PIC X(13).
             10 PI-DRKI2198-VZAVETLFDNR   PIC S9(4) COMP.

      *
       01  PO-DRKI2198.
           05  PO-DRKI2198-RC                 PIC  X(003).
               88 PO-DRKI2198-RC-OK                        VALUE 'OK '.
               88 PO-DRKI2198-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRKI2198-DATEN.


             10 PO-DRKI2198-LETZTEAENDERUNG   PIC X(26).
             10 PO-DRKI2198-VZAEINZVTGHOSTID   PIC X(15).
             10 PO-DRKI2198-VZANUMMER   PIC S9(9)V COMP-3.
             10 PO-DRKI2198-VZAZUABSCHLAGTXT   PIC X(30).
             10 PO-DRKI2198-VZAZUABSCHLKZ   PIC X(3).
             10 PO-DRKI2198-VZAZUABSCHLPROZ   PIC S9(2)V9(2) COMP-3.
             10 PO-DRKI2198-VZAZUABSCHLWERT   PIC S9(7)V9(2) COMP-3.


      *
      ******* ENDE COPY-MEMBER **************************************
