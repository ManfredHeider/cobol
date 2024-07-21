      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WTKI0044.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\WTKI0044.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 13:00:00   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Aug 1998 16:56:16   T13EX19
      *VB-SCHNITTSTELLE
      *
      *   Rev 1.0   18 Aug 1997 11:12:34   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *                                                               *
      *    COPY-MEMBER/ARBEITSBEREICH FÜR DIE DB2-TABLE               *
      *                                                               *
      *    PERSONADRESSBEZ                                            *
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
        05  WB-NI-PERSONADRESSBEZ-TAB.
           10 WB-NI-PERSONADRESSBEZ-ELE PIC S9(04) COMP OCCURS 4 TIMES.
        05  WB-NI-PERSONADRESSBEZ REDEFINES WB-NI-PERSONADRESSBEZ-TAB.
           10 WB-NI-PERSABPERSONENID      PIC S9(4) COMP.
           10 WB-NI-PERSABADRESSEID       PIC S9(4) COMP.
           10 WB-NI-PERSABADRESSARTID     PIC S9(4) COMP.
           10 WB-NI-LETZTEAENDERUNG       PIC S9(4) COMP.
        05  DCL-PERSONADRESSBEZ.
           10 WB-PERSABPERSONENID         PIC X(13).
           10 WB-PERSABADRESSEID          PIC X(13).
           10 WB-PERSABADRESSARTID        PIC S9(4) COMP.
           10 LETZTEAENDERUNG             PIC X(26).
      ******************************************************************
