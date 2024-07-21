      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  29.09.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2195.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2195.CPv  $
      *
      *   Rev 2.2   Jun 21 2008 12:53:20   t13ex52
      *AKL00665: Allfinanz; Firmenkunden AMV
      *
      *   Rev 2.0   29 Sep 2007 12:47:48   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2195                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2195                    *
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
      *    FÜR DIE FUNKTION DRKI2195
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2195.
           05  PI-DRKI2195-KEY.


             10 PI-DRKI2195-VZAUNIVLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2195-VZAVERTRAGSID   PIC X(13).
             10 PI-DRKI2195-VZAVETLFDNR   PIC S9(4) COMP.


           05  PI-DRKI2195-DATEN.


             10 PI-DRKI2195-VZAEINZVTGHOSTID   PIC X(15).
             10 PI-DRKI2195-VZANUMMER   PIC S9(9)V COMP-3.
             10 PI-DRKI2195-VZAZUABSCHLAGTXT   PIC X(30).
             10 PI-DRKI2195-VZAZUABSCHLKZ   PIC X(3).
             10 PI-DRKI2195-VZAZUABSCHLPROZ   PIC S9(2)V9(2) COMP-3.
             10 PI-DRKI2195-VZAZUABSCHLWERT   PIC S9(7)V9(2) COMP-3.

      *
       01  PO-DRKI2195.
           05  PO-DRKI2195-RC                 PIC  X(003).
               88 PO-DRKI2195-RC-OK                        VALUE 'OK '.
               88 PO-DRKI2195-RC-DUPLICATE-VALUE           VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************
