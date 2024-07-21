      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  15.09.2003
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1583.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI1583.CPv  $
      *
      *   Rev 2.1   Sep 30 2003 10:22:10   t13ex12
      *AKI00367 Neukundenermittlung
      *
      *   Rev 2.0   Sep 15 2003 16:29:50   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1583                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI1583                    *
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
      *    FÜR DIE FUNKTION DRKI1583
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1583.
           05  PI-DRKI1583-KEY.


             10 PI-DRKI1583-NKDLFDNR   PIC S9(4) COMP.
             10 PI-DRKI1583-NKDPERSONENID   PIC X(13).


           05  PI-DRKI1583-DATEN.


             10 PI-DRKI1583-NKDBWTUNGSKZDVAG   PIC X(2).
             10 PI-DRKI1583-NKDBWTUNGSKZVB   PIC X(02).
             10 PI-DRKI1583-NKDEREIGNISKZ   PIC S9(4) COMP.
             10 PI-DRKI1583-NKDKNDSTATUSIDALT   PIC X(01).
             10 PI-DRKI1583-NKDKNDSTATUSIDNEU   PIC X(1).
             10 PI-DRKI1583-NKDKOMMISARISCHKZ   PIC X(1).
             10 PI-DRKI1583-NKDKUNDENNUMMER   PIC S9(9) COMP.
             10 PI-DRKI1583-NKDPARTGESELLSCHID   PIC S9(4) COMP.
             10 PI-DRKI1583-NKDPGMNAME   PIC X(8).
             10 PI-DRKI1583-NKDPRODUKT   PIC X(10).
             10 PI-DRKI1583-NKDTAGESDATUM   PIC X(10).
             10 PI-DRKI1583-NKDVBNR         PIC S9(9) COMP.
             10 PI-DRKI1583-NKDDIREKTION    PIC S9(4) COMP.
             10 PI-DRKI1583-NKDVERTRAGSNUMMER   PIC X(15).


      *
       01  PO-DRKI1583.
           05  PO-DRKI1583-RC                 PIC  X(003).
               88 PO-DRKI1583-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1583-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
