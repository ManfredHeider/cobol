      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  17.09.2003
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1585.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI1585.CPv  $
      *
      *   Rev 2.1   Sep 30 2003 10:22:46   t13ex12
      *AKI00367 Neukundenermittlung
      *
      *   Rev 2.0   Sep 17 2003 10:19:36   t13ex39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1585                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI1585                    *
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
      *    FÜR DIE FUNKTION DRKI1585
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1585.
           05  PI-DRKI1585-KEY.


             10 PI-DRKI1585-NKDLFDNR   PIC S9(4) COMP.
             10 PI-DRKI1585-NKDPERSONENID   PIC X(13).

      *
       01  PO-DRKI1585.
           05  PO-DRKI1585-RC                 PIC  X(003).
               88 PO-DRKI1585-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1585-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRKI1585-DATEN.


             10 PO-DRKI1585-NKDBWTUNGSKZDVAG   PIC X(2).
             10 PO-DRKI1585-NKDBWTUNGSKZVB   PIC X(02).
             10 PO-DRKI1585-NKDEREIGNISKZ   PIC S9(4) COMP.
             10 PO-DRKI1585-NKDKNDSTATUSIDALT   PIC X(01).
             10 PO-DRKI1585-NKDKNDSTATUSIDNEU   PIC X(1).
             10 PO-DRKI1585-NKDKOMMISARISCHKZ   PIC X(1).
             10 PO-DRKI1585-NKDKUNDENNUMMER   PIC S9(9) COMP.
             10 PO-DRKI1585-NKDPARTGESELLSCHID   PIC S9(4) COMP.
             10 PO-DRKI1585-NKDPGMNAME   PIC X(8).
             10 PO-DRKI1585-NKDPRODUKT   PIC X(10).
             10 PO-DRKI1585-NKDTAGESDATUM   PIC X(10).
             10 PO-DRKI1585-NKDVBNR       PIC S9(9) COMP.
             10 PO-DRKI1585-NKDDIREKTION  PIC S9(4) COMP.
             10 PO-DRKI1585-NKDVERTRAGSNUMMER   PIC X(15).


      *
      ******* ENDE COPY-MEMBER **************************************
