      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  15.09.2003
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1582.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI1582.CPv  $
      *
      *   Rev 2.1   Sep 30 2003 10:21:52   t13ex12
      *AKI00367 Neukundenermittlung
      *
      *   Rev 2.0   Sep 15 2003 16:29:38   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1582                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI1582                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  24.09.2003/14:24:49
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
      *    FÜR DIE FUNKTION DRKI1582
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1582.
           05  PI-DRKI1582-KEY.


             10 PI-DRKI1582-NKDLFDNR   PIC S9(4) COMP.
             10 PI-DRKI1582-NKDPERSONENID   PIC X(13).


           05  PI-DRKI1582-DATEN.


             10 PI-DRKI1582-NKDBWTUNGSKZDVAG   PIC X(2).
             10 PI-DRKI1582-NKDBWTUNGSKZVB   PIC X(02).
             10 PI-DRKI1582-NKDEREIGNISKZ   PIC S9(4) COMP.
             10 PI-DRKI1582-NKDKNDSTATUSIDALT   PIC X(01).
             10 PI-DRKI1582-NKDKNDSTATUSIDNEU   PIC X(1).
             10 PI-DRKI1582-NKDKOMMISARISCHKZ   PIC X(1).
             10 PI-DRKI1582-NKDKUNDENNUMMER   PIC S9(9) COMP.
             10 PI-DRKI1582-NKDPARTGESELLSCHID   PIC S9(4) COMP.
             10 PI-DRKI1582-NKDPGMNAME   PIC X(8).
             10 PI-DRKI1582-NKDPRODUKT   PIC X(10).
             10 PI-DRKI1582-NKDTAGESDATUM   PIC X(10).
             10 PI-DRKI1582-NKDVBNR         PIC S9(9) COMP.
             10 PI-DRKI1582-NKDDIREKTION    PIC S9(4) COMP.
             10 PI-DRKI1582-NKDVERTRAGSNUMMER   PIC X(15).

      *
       01  PO-DRKI1582.
           05  PO-DRKI1582-RC                 PIC  X(003).
               88 PO-DRKI1582-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1582-RC-DUPLICATE-VALUE           VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************
