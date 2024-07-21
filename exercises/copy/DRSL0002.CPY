      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRSL0002.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRSL0002.CPv  $
      *
      *   Rev 2.1   Mar 03 2000 10:03:48   T13AE30
      *ASL00050 - OLB
      *
      *   Rev 2.0   Feb 11 2000 15:19:44   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRSL0002                                      *
      *    --------------------------------------------------------   *
      *    << OLB-DATEI>> SATZBESCHREIBUNG                            *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WILLIG                                           *
      *    DATUM:    03.03.00                                         *
      *****************************************************************
      *    VERSION  1
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * ASL00050!03.03.00! 01 ! NEUERSTELLUNG             ! WILLIG    *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *
       01  DRSL0002.
           05  DRSL0002-REC.
               10  DRSL0002-KEY.
                   15  DRSL0002-VBNR                 PIC 9(007).
                   15  DRSL0002-QUARTAL              PIC 9(001).
               10  DRSL0002-BESTANDSKZ               PIC 9(001).
               10  DRSL0002-DIR                      PIC 9(003).
               10  DRSL0002-VB-STUFE                 PIC X(004).
               10  DRSL0002-NAME                     PIC X(020).
               10  DRSL0002-OLB-TAB    OCCURS 3.
                   15  DRSL0002-OLB                  PIC S9(005) COMP-3.
                   15  DRSL0002-OLB-KZ               PIC X(001).

      ******************** ENDE COPY-MEMBER DRSL0002*******************
