      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRSL0001.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\DRSL0001.CPv  $
      *
      *   Rev 2.2   Mar 03 2000 10:11:32   T13AE30
      *ASL00050 - OLB, 7er STUFEN EINGEFÜHRT
      *
      *   Rev 2.1   Mar 02 2000 11:03:12   T13AE30
      *ASL00050 - OLB
      *
      *   Rev 2.0   Feb 11 2000 15:19:44   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRSL0001                                      *
      *    --------------------------------------------------------   *
      *    << AUFBAU-OLZ>> SATZBESCHREIBUNG                           *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WILLIG                                           *
      *    DATUM:    02.03.00                                         *
      *****************************************************************
      *    VERSION  1
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * ASL00050!02.03.00! 01 ! NEUERSTELLUNG             ! WILLIG    *
      * ASL00050!03.03.00! 02 ! 07er STUFE EINGEFÜHRT     ! WILLIG    *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *
       01 DRSL0001.
         05 DRSL0001-REC.
           07 DRSL0001-UEBERTRAG.
             10 DRSL0001-KEY.
                 15  DRSL0001-VBNR                   PIC 9(007).
                 15  DRSL0001-QUARTAL                PIC 9(001).
             10  DRSL0001-BESTANDSKZ                 PIC 9(001).
             10  DRSL0001-DIR                        PIC 9(003).
             10  DRSL0001-VB-STUFE                   PIC X(004).
             10  DRSL0001-NAME                       PIC X(020).
             10  DRSL0001-AUFBAU-OLZ-TAB    OCCURS 3.
                 15  DRSL0001-AUFBAU-OLZ             PIC S9(005) COMP-3.
                 15  DRSL0001-AUFBAU-OLZ-KZ          PIC X(001).
           07 DRSL0001-AUFBAU-NEU.
             10 DRSL0001-NEU-KZ                      PIC X(001).
             10 DRSL0001-UMSTUFDAT                   PIC 9(007)  COMP-3.

      ******************** ENDE COPY-MEMBER DRSL0001*******************
