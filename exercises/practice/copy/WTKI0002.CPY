      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WTKI0002.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\WTKI0002.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:59:48   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Aug 1998 16:56:06   T13EX19
      *VB-SCHNITTSTELLE
      *
      *   Rev 1.0   18 Aug 1997 11:09:20   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *                                                               *
      *    COPY-MEMBER/ARBEITSBEREICH FÜR DIE DB2-TABLE               *
      *                                                               *
      *    ADRESSE                                                    *
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
        05  WB-NI-ADRESSE-TAB.
           10 WB-NI-ADRESSE-ELE PIC S9(04) COMP OCCURS 8 TIMES.
        05  WB-NI-ADRESSE REDEFINES WB-NI-ADRESSE-TAB.
           10 WB-NI-ADRESSEID             PIC S9(4) COMP.
           10 WB-NI-ADRESSEBDSLDID        PIC S9(4) COMP.
           10 WB-NI-ADRESSELANDID         PIC S9(4) COMP.
           10 WB-NI-ADRESSEORT            PIC S9(4) COMP.
           10 WB-NI-ADRESSEPLZ            PIC S9(4) COMP.
           10 WB-NI-ADRESSESTRHAUSNUM     PIC S9(4) COMP.
           10 WB-NI-ADRESSEISTVTADR       PIC S9(4) COMP.
           10 WB-NI-LETZTEAENDERUNG       PIC S9(4) COMP.
        05  DCL-ADRESSE.
           10 WB-ADRESSEID                PIC X(13).
           10 WB-ADRESSEBDSLDID           PIC X(3).
           10 WB-ADRESSELANDID            PIC X(3).
           10 WB-ADRESSEORT               PIC X(30).
           10 WB-ADRESSEPLZ               PIC X(15).
           10 WB-ADRESSESTRHAUSNUM        PIC X(45).
           10 WB-ADRESSEISTVTADR          PIC X(01).
           10 LETZTEAENDERUNG             PIC X(26).
      ******************************************************************
