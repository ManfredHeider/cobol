      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  29.09.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2185.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2185.CPv  $
      *
      *   Rev 2.2   Jun 21 2008 12:52:34   t13ex52
      *AKL00665: Allfinanz; Firmenkunden AMV
      *
      *   Rev 2.0   29 Sep 2007 12:18:38   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2185                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2185                    *
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
      *    FÜR DIE FUNKTION DRKI2185
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2185.
           05  PI-DRKI2185-KEY.


             10 PI-DRKI2185-VRIRISIKOGRUPPID   PIC S9(4) COMP.
             10 PI-DRKI2185-VRIRISIKOLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2185-VRITHEMGEBSL-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI2185-VRIVERTRAGSID   PIC X(13).
             10 PI-DRKI2185-VRIVETLFDNR   PIC S9(4) COMP.


           05  PI-DRKI2185-DATEN.


             10 PI-DRKI2185-VRIEINZVTGHOSTID   PIC X(15).
             10 PI-DRKI2185-VRIMERKMAL   PIC S9(9)V COMP-3.
             10 PI-DRKI2185-VRIRSKBETRAG   PIC S9(7)V9(2) COMP-3.
             10 PI-DRKI2185-VRIRSKENTGRZ-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI2185-VRIRSKENTSCHGREN   PIC S9(8)V9(2) COMP-3.
             10 PI-DRKI2185-VRIRSKKENNZIFFER   PIC X(10).
             10 PI-DRKI2185-VRIRSKMENGE   PIC S9(9) COMP.
             10 PI-DRKI2185-VRIRSKMENGEEINH   PIC X(25).
             10 PI-DRKI2185-VRIRSKMITVERSKZ   PIC X(2).
             10 PI-DRKI2185-VRIRSKNAME   PIC X(50).
             10 PI-DRKI2185-VRIRSKSATZ   PIC S9(3)V9(7) COMP-3.
             10 PI-DRKI2185-VRIRSKSATZEINH   PIC S9(9) COMP.
             10 PI-DRKI2185-VRIRSKSUMME   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI2185-VRIRSKSUMMEEINH   PIC X(20).
             10 PI-DRKI2185-VRIRSKSUMUNBEGRZ   PIC X(2).
             10 PI-DRKI2185-VRIRSKTEXT   PIC X(30).

      *
       01  PO-DRKI2185.
           05  PO-DRKI2185-RC                 PIC  X(003).
               88 PO-DRKI2185-RC-OK                        VALUE 'OK '.
               88 PO-DRKI2185-RC-DUPLICATE-VALUE           VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************
