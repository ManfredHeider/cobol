      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  29.09.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2186.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2186.CPv  $
      *
      *   Rev 2.2   Jun 21 2008 12:52:44   t13ex52
      *AKL00665: Allfinanz; Firmenkunden AMV
      *
      *   Rev 2.0   29 Sep 2007 12:21:34   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2186                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2186                    *
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
      *    FÜR DIE FUNKTION DRKI2186
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2186.
           05  PI-DRKI2186-KEY.


             10 PI-DRKI2186-VRIRISIKOGRUPPID   PIC S9(4) COMP.
             10 PI-DRKI2186-VRIRISIKOLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2186-VRITHEMGEBSL-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI2186-VRIVERTRAGSID   PIC X(13).
             10 PI-DRKI2186-VRIVETLFDNR   PIC S9(4) COMP.


           05  PI-DRKI2186-DATEN.


             10 PI-DRKI2186-VRIEINZVTGHOSTID   PIC X(15).
             10 PI-DRKI2186-VRIMERKMAL   PIC S9(9)V COMP-3.
             10 PI-DRKI2186-VRIRSKBETRAG   PIC S9(7)V9(2) COMP-3.
             10 PI-DRKI2186-VRIRSKENTGRZ-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI2186-VRIRSKENTSCHGREN   PIC S9(8)V9(2) COMP-3.
             10 PI-DRKI2186-VRIRSKKENNZIFFER   PIC X(10).
             10 PI-DRKI2186-VRIRSKMENGE   PIC S9(9) COMP.
             10 PI-DRKI2186-VRIRSKMENGEEINH   PIC X(25).
             10 PI-DRKI2186-VRIRSKMITVERSKZ   PIC X(2).
             10 PI-DRKI2186-VRIRSKNAME   PIC X(50).
             10 PI-DRKI2186-VRIRSKSATZ   PIC S9(3)V9(7) COMP-3.
             10 PI-DRKI2186-VRIRSKSATZEINH   PIC S9(9) COMP.
             10 PI-DRKI2186-VRIRSKSUMME   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI2186-VRIRSKSUMMEEINH   PIC X(20).
             10 PI-DRKI2186-VRIRSKSUMUNBEGRZ   PIC X(2).
             10 PI-DRKI2186-VRIRSKTEXT   PIC X(30).


      *
       01  PO-DRKI2186.
           05  PO-DRKI2186-RC                 PIC  X(003).
               88 PO-DRKI2186-RC-OK                        VALUE 'OK '.
               88 PO-DRKI2186-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
