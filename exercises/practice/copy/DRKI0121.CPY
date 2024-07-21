      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0121.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRKI0121.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:53:00   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   02 Jun 1998 14:07:26   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   29 Sep 1997 11:49:06   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *                                                                *
      *    COPY-MEMBER ZU DEM AUFBEREITUNGSPROGRAM PMKI0075 CKV        *
      *        - Ausgabe CKV VERSKRANKENTARIF  DATEN               -   *
      *                                                                *
      *    SATZLÄNGE  46                                               *
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
      *13.03.98!NEUERSTELLUNG                          !Huebner  ! 01  *
      *KI-NEU  !                                       !         !     *
      *        !                                       !         !     *
      *        !                                       !         !     *
      ******************************************************************
         05 VERSKRANKENTARIF.
      *
           10  VKT-GES                       PIC 9(02).
           10  VKT-VNR                       PIC 9(15).
           10  VKT-TARLFDNR                  PIC S9(4) COMP.
           10  VKT-PNR                       PIC S9(4) COMP.
           10  VKT-BEITRAGVZ                 PIC 99V9.
           10  VKT-GESCHAEFTSID              PIC S9(4) COMP.
           10  VKT-GESCHAEFTSST              PIC X(20).
