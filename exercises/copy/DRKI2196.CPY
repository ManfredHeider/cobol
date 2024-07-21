      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  29.09.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2196.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2196.CPv  $
      *
      *   Rev 2.2   Jun 21 2008 12:53:28   t13ex52
      *AKL00665: Allfinanz; Firmenkunden AMV
      *
      *   Rev 2.0   29 Sep 2007 12:50:44   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2196                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2196                    *
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
      *    FÜR DIE FUNKTION DRKI2196
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2196.
           05  PI-DRKI2196-KEY.


             10 PI-DRKI2196-VZAUNIVLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2196-VZAVERTRAGSID   PIC X(13).
             10 PI-DRKI2196-VZAVETLFDNR   PIC S9(4) COMP.


           05  PI-DRKI2196-DATEN.


             10 PI-DRKI2196-VZAEINZVTGHOSTID   PIC X(15).
             10 PI-DRKI2196-VZANUMMER   PIC S9(9)V COMP-3.
             10 PI-DRKI2196-VZAZUABSCHLAGTXT   PIC X(30).
             10 PI-DRKI2196-VZAZUABSCHLKZ   PIC X(3).
             10 PI-DRKI2196-VZAZUABSCHLPROZ   PIC S9(2)V9(2) COMP-3.
             10 PI-DRKI2196-VZAZUABSCHLWERT   PIC S9(7)V9(2) COMP-3.


      *
       01  PO-DRKI2196.
           05  PO-DRKI2196-RC                 PIC  X(003).
               88 PO-DRKI2196-RC-OK                        VALUE 'OK '.
               88 PO-DRKI2196-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
