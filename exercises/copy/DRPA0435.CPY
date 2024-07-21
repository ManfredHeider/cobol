      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0435.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA0435.CPv  $
      *
      *   Rev 2.1   Sep 28 2017 14:54:10   T13AE34
      *ARZ00049: Reaktivieren drpa0434 drpa0435
      *
      *   Rev 2.0   Feb 27 1999 14:29:02   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   06 Aug 1998 15:08:42   T01JM2S
      *APAA0032 - PEW-RELEASE 2.0
      *
      *   Rev 1.0   23 Jul 1997 12:36:24   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0435                                      *
      *    --------------------------------------------------------   *
      *    PEW-STRUKTUR-DATEN                                         *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    11.08.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!11.08.97! 01 ! NEU                       ! WOLFF    *
      *         !        !    !                           !          *
      * PEW-REL.!15.05.98!    ! EINBAU PRZ                ! WOLFF    *
      * 2.0     !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE DATEN DER PEW-STRUKTUR-DATEN      *
      *                                                              *
      ****************************************************************
      *
             30 PEW-STRUKTUR-TABELLE.
                35 PEW-STRUKTUR                     OCCURS 20
                                                    INDEXED BY IX-PE-S.
                   40 MA-ST-NR                      PIC S9(009) BINARY.
150598             40 PRZ                           PIC  X(001).
                   40 EMP-STUFE                     PIC  X(004).
                   40 DIFF-PROV-STUFE               PIC S9(004) BINARY.
                   40 KNR-EMP-TYP                   PIC S9(004) BINARY.
      *
      ******* ENDE COPY-MEMBER **************************************

