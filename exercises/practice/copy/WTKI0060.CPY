      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   wtki0060.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\wtki0060.cpv  $
      *
      *   Rev 2.1   Apr 15 1999 16:07:08   T13EX19
      *aki00020   admasperre eingefügt
      *
      *   Rev 2.0   Feb 27 1999 13:00:04   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Aug 1998 16:56:22   T13EX19
      *VB-SCHNITTSTELLE
      *
      *   Rev 1.0   18 Aug 1997 11:13:54   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *                                                               *
      *    COPY-MEMBER/ARBEITSBEREICH FÜR DIE DB2-TABLE               *
      *                                                               *
      *    AUSSENDIENSTMA                                             *
      *                                                               *
      *****************************************************************
      *    VERSION   1    31.03.94 18:12:47
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME    *
      * -------------------------------------------------------------*
      * AKI00001!10.07.98!01.01! NEU                       !Zgur-Vujic
      * --------!--------!-----!---------------------------!---------*
      * aki00018!15.04.99!     !ADMASPERRE eingefügt       !VZG      *
      *         !        !     !                           !         *
      *         !        !     !                           !         *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
        05  WB-NI-AUSSENDIENSTMA-TAB.
           10 WB-NI-AUSSENDIENSTMA-ELE PIC S9(04) COMP OCCURS 11 TIMES.
        05  WB-NI-AUSSENDIENSTMA REDEFINES WB-NI-AUSSENDIENSTMA-TAB.
           10 WB-NI-ADMAPERSONENID        PIC S9(4) COMP.
           10 WB-NI-ADMAPERSONENROLLE     PIC S9(4) COMP.
           10 WB-NI-ADMASTRUKTURSTUFE     PIC S9(4) COMP.
           10 WB-NI-ADMAAUSTRITTSDATUM    PIC S9(4) COMP.
           10 WB-NI-ADMADIREKTION         PIC S9(4) COMP.
           10 WB-NI-ADMARUECKSTUFDATUM    PIC S9(4) COMP.
           10 WB-NI-ADMAUMSTUFUNGSDAT     PIC S9(4) COMP.
           10 WB-NI-ADMAINPROBEKZ         PIC S9(4) COMP.
           10 WB-NI-ADMAPRAXISPROVSATZ    PIC S9(4) COMP.
           10 WB-NI-ADMAARBSTATIONID      PIC S9(4) COMP.
           10 WB-NI-ADMASPERRE            PIC S9(4) COMP.
        05  DCL-AUSSENDIENSTMA.
           10 WB-ADMAPERSONENID           PIC X(13).
           10 WB-ADMAPERSONENROLLE        PIC X(01).
           10 WB-ADMASTRUKTURSTUFE        PIC S9(9) COMP.
           10 WB-ADMAAUSTRITTSDATUM       PIC X(10).
           10 WB-ADMADIREKTION            PIC S9(4) COMP.
           10 WB-ADMARUECKSTUFDATUM       PIC X(10).
           10 WB-ADMAUMSTUFUNGSDAT        PIC X(10).
           10 WB-ADMAINPROBEKZ            PIC X(01).
           10 WB-ADMAPRAXISPROVSATZ       PIC S9(3)V9(2) COMP-3.
           10 WB-ADMAARBSTATIONID         PIC S9(9) COMP.
           10 WB-ADMASPERRE               PIC X(01).
      ******************************************************************
