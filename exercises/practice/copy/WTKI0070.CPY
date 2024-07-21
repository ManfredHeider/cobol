      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WTKI0070.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\WTKI0070.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 13:00:06   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Aug 1998 16:56:28   T13EX19
      *VB-SCHNITTSTELLE
      *
      *   Rev 1.0   18 Aug 1997 11:14:44   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *                                                               *
      *    COPY-MEMBER/ARBEITSBEREICH FÜR DIE DB2-TABLE               *
      *                                                               *
      *    JURISTISCHEPERSON                                          *
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
        05  WB-NI-JURISTISCHEPERSON-TAB.
           10 WB-NI-JURISTISCHEPERSON-ELE PIC S9(04) COMP
                                          OCCURS 12 TIMES.
        05  WB-NI-JURISTISCHEPERSON
                             REDEFINES WB-NI-JURISTISCHEPERSON-TAB.
           10 WB-NI-JUPPERSONENID         PIC S9(4) COMP.
           10 WB-NI-JUPBERECHGEBUEHR      PIC S9(4) COMP.
           10 WB-NI-JUPBERGEBUEHRWKZ      PIC S9(4) COMP.
           10 WB-NI-JUPERFASSTAM          PIC S9(4) COMP.
           10 WB-NI-JUPJAHRESUMSATZ       PIC S9(4) COMP.
           10 WB-NI-JUPJAHRUMSATZWKZ      PIC S9(4) COMP.
           10 WB-NI-JUPEINKOSTEUER        PIC S9(4) COMP.
           10 WB-NI-JUPKIRCHENSTEUER      PIC S9(4) COMP.
           10 WB-NI-JUPKOERPERSTEUER      PIC S9(4) COMP.
           10 WB-NI-JUPGEWERBESTEUER      PIC S9(4) COMP.
           10 WB-NI-JUPGESAMTSTEUER       PIC S9(4) COMP.
           10 WB-NI-JUPRECHTSFORMID       PIC S9(4) COMP.
        05  DCL-JURISTISCHEPERSON.
           10 WB-JUPPERSONENID            PIC X(13).
           10 WB-JUPBERECHGEBUEHR         PIC S9(9)V9(2) COMP-3.
           10 WB-JUPBERGEBUEHRWKZ         PIC X(01).
           10 WB-JUPERFASSTAM             PIC X(10).
           10 WB-JUPJAHRESUMSATZ          PIC S9(9)V9(2) COMP-3.
           10 WB-JUPJAHRUMSATZWKZ         PIC X(01).
           10 WB-JUPEINKOSTEUER           PIC S9(3)V9(2) COMP-3.
           10 WB-JUPKIRCHENSTEUER         PIC S9(3)V9(2) COMP-3.
           10 WB-JUPKOERPERSTEUER         PIC S9(3)V9(2) COMP-3.
           10 WB-JUPGEWERBESTEUER         PIC S9(3)V9(2) COMP-3.
           10 WB-JUPGESAMTSTEUER          PIC S9(3)V9(2) COMP-3.
           10 WB-JUPRECHTSFORMID          PIC S9(4) COMP.
      ******************************************************************
