      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0608.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRKI0608.CPv  $
      *
      *   Rev 2.0   Apr 15 1999 11:32:04   T13EX50
      *Produktionsübergabe - CKV
      *
      *   Rev 1.1   Mar 25 1999 10:37:06   T13EX50
      *Erste Version 
      *
      *   Rev 1.0   25 Aug 1998 09:22:58   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *                                                                *
      *    COPY-MEMBER DRKI0608                             CKV        *
      *        -  CKV VERSKRANKENTARIF  DATEN                      -   *
      *                                                                *
      *    SATZLÄNGE  47                                               *
      *                                                                *
      *    Anmerkung:                                                  *
      *                                                                *
      *                                                                *
      *                                                                *
      *                                                                *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                         !NAME     !VERS.*
      *AUFTRAG !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *21.09.98!NEUERSTELLUNG                          !Sebregond! 01  *
      *KI-NEU  !                                       !i        !     *
      *        !                                       !         !     *
      *        !                                       !         !     *
      ******************************************************************
         01 DRKI0608.
      *
           10  VKT-GES                       PIC 9(02).
           10  VKT-VNR                       PIC 9(15).
           10  VKT-TARLFDNR                  PIC 9(04).
           10  VKT-PNR                       PIC 9(02).
           10  VKT-BEITRAGVZ                 PIC 99V9.
           10  VKT-GESCHAEFTSID              PIC S9(4) COMP.
           10  VKT-GESCHAEFTSST              PIC X(20).
           10  VKT-AKTUAL-KZ                 PIC X(01).
      *
      *        I = INSERT
      *        U = UPDATE
      *        D = DELETE
      *****************************************************************
      *        E N D E  COPY-BUCH   DRKI0608  VERSKRANKENTARIF        *
      *****************************************************************
