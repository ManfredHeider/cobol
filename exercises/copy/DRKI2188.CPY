      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  29.09.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2188.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2188.CPv  $
      *
      *   Rev 2.2   Jun 21 2008 12:53:00   t13ex52
      *AKL00665: Allfinanz; Firmenkunden AMV
      *
      *   Rev 2.0   29 Sep 2007 12:27:24   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2188                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2188                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  28.05.2008/16:31:30
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
      *    FÜR DIE FUNKTION DRKI2188
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2188.
           05  PI-DRKI2188-KEY.


             10 PI-DRKI2188-VRIRISIKOGRUPPID   PIC S9(4) COMP.
             10 PI-DRKI2188-VRIRISIKOLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2188-VRITHEMGEBSL-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI2188-VRIVERTRAGSID   PIC X(13).
             10 PI-DRKI2188-VRIVETLFDNR   PIC S9(4) COMP.

      *
       01  PO-DRKI2188.
           05  PO-DRKI2188-RC                 PIC  X(003).
               88 PO-DRKI2188-RC-OK                        VALUE 'OK '.
               88 PO-DRKI2188-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRKI2188-DATEN.


             10 PO-DRKI2188-LETZTEAENDERUNG   PIC X(26).
             10 PO-DRKI2188-VRIEINZVTGHOSTID   PIC X(15).
             10 PO-DRKI2188-VRIMERKMAL   PIC S9(9)V COMP-3.
             10 PO-DRKI2188-VRIRSKBETRAG   PIC S9(7)V9(2) COMP-3.
             10 PO-DRKI2188-VRIRSKENTGRZ-KNR   PIC S9(15)V COMP-3.
             10 PO-DRKI2188-VRIRSKENTGRZ-KTY   PIC S9(4) COMP.
             10 PO-DRKI2188-VRIRSKENTSCHGREN   PIC S9(8)V9(2) COMP-3.
             10 PO-DRKI2188-VRIRSKKENNZIFFER   PIC X(10).
             10 PO-DRKI2188-VRIRSKMENGE   PIC S9(9) COMP.
             10 PO-DRKI2188-VRIRSKMENGEEINH   PIC X(25).
             10 PO-DRKI2188-VRIRSKMITVERSKZ   PIC X(2).
             10 PO-DRKI2188-VRIRSKNAME   PIC X(50).
             10 PO-DRKI2188-VRIRSKSATZ   PIC S9(3)V9(7) COMP-3.
             10 PO-DRKI2188-VRIRSKSATZEINH   PIC S9(9) COMP.
             10 PO-DRKI2188-VRIRSKSUMME   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI2188-VRIRSKSUMMEEINH   PIC X(20).
             10 PO-DRKI2188-VRIRSKSUMUNBEGRZ   PIC X(2).
             10 PO-DRKI2188-VRIRSKTEXT   PIC X(30).
             10 PO-DRKI2188-VRITHEMGEBSL-KTY   PIC S9(4) COMP.


      *
      ******* ENDE COPY-MEMBER **************************************
