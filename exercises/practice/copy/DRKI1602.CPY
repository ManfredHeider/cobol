      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.10.2003
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1602.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI1602.CPv  $
      *
      *   Rev 2.1   Jan 12 2004 10:31:38   t13ex27
      *AKI00384 - EA-Anforderungen und IOS
      *
      *   Rev 2.0   Oct 28 2003 17:47:28   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1602                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI1602                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  29.10.2003/15:47:37
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
      *    FÜR DIE FUNKTION DRKI1602
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1602.
           05  PI-DRKI1602-KEY.

             10 PI-DRKI1602-EADATUM   PIC X(10).
             10 PI-DRKI1602-EAPCNR    PIC S9(9) COMP.

           05  PI-DRKI1602-DATEN.

             10 PI-DRKI1602-EADIREKTION      PIC S9(4) COMP.
             10 PI-DRKI1602-EAGRUNDID        PIC S9(4) COMP.
             10 PI-DRKI1602-EAKZERL          PIC X(1).
             10 PI-DRKI1602-EAPCBESITZERID   PIC X(13).
             10 PI-DRKI1602-EAPCTYP          PIC X(1).
             10 PI-DRKI1602-EAPGM            PIC X(8).
             10 PI-DRKI1602-EATESTLAUF       PIC X(1).
             
      *
       01  PO-DRKI1602.
           05  PO-DRKI1602-RC                 PIC  X(003).
               88 PO-DRKI1602-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1602-RC-DUPLICATE-VALUE           VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************
