      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WTKI0030.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\WTKI0030.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:59:56   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Aug 1998 16:56:08   T13EX19
      *VB-SCHNITTSTELLE
      *
      *   Rev 1.0   18 Aug 1997 11:11:28   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *                                                               *
      *    COPY-MEMBER/ARBEITSBEREICH FÜR DIE DB2-TABLE               *
      *                                                               *
      *    WB-KOMMUNIKATION                                           *
      *                                                               *
      *****************************************************************
      *    VERSION   1    31.03.94 18:12:47
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME    *
      * -------------------------------------------------------------*
      * AKI00001!10.07.98!01.01! NEU                       !Zgur-Vujic
      * --------!--------!-----!---------------------------!---------*
      *         !        !     !                           !         *
      *         !        !     !                           !         *
      *         !        !     !                           !         *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
        05  WB-NI-KOMMUNIKATION-TAB.
           10 WB-NI-KOMMUNIKATION-ELE PIC S9(04) COMP OCCURS 5 TIMES.
        05  WB-NI-KOMMUNIKATION REDEFINES WB-NI-KOMMUNIKATION-TAB.
           10 WB-NI-KOMMPERSONENID        PIC S9(4) COMP.
           10 WB-NI-KOMMTELARTID          PIC S9(4) COMP.
           10 WB-NI-KOMMLAUFENDENUMMER    PIC S9(4) COMP.
           10 WB-NI-KOMMNUMMER            PIC S9(4) COMP.
           10 WB-NI-LETZTEAENDERUNG       PIC S9(4) COMP.
        05  DCL-KOMMUNIKATION.
           10 WB-KOMMPERSONENID           PIC X(13).
           10 WB-KOMMTELARTID             PIC S9(4) COMP.
           10 WB-KOMMLAUFENDENUMMER       PIC S9(9) COMP.
           10 WB-KOMMNUMMER.
              49 WB-KOMMNUMMER-LEN        PIC S9(4) COMP.
              49 WB-KOMMNUMMER-TEXT       PIC X(128).
           10 LETZTEAENDERUNG             PIC X(26).
      ******************************************************************
