      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1107.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA1107.CPv  $
      *
      *   Rev 2.0   May 26 1999 13:51:34   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   20 Jul 1998 10:21:40   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *
      ****************************************************************
      *    COPY-MEMBER: DRPA1107 FUNKTION VM IN STRUKTUR ANPASSEN    *
      *                          FüR UWPA0021                        *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    27.10.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!27.10.98! 01 !NEU                        !BOHLINGER *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1107                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1107.
          05 PI-DRPA1107-UFKT                    PIC  9(02).
          05 PI-DRPA1107-VM                      PIC  9(007).


       01 PO-DRPA1107.
          05 PO-DRPA1107-RC                    PIC  X(003).
             88 PO-DRPA1107-RC-OK                       VALUE 'OK '.
             88 PO-DRPA1107-RC-VM-UNZULAESSIG           VALUE 'VMU'.
             88 PO-DRPA1107-RC-VM-BER-IN-PE-S           VALUE 'VBE'.
             88 PO-DRPA1107-RC-KEIN-AVERM-VORH          VALUE 'KAV'.
             88 PO-DRPA1107-RC-KEIN-VM-VORH             VALUE 'KVM'.
      *
      ******** Ende Copy-Member **************************************
