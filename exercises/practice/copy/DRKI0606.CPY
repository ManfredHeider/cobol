      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0606.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI0606.CPv  $
      *
      *   Rev 2.3   Jun 27 2012 13:36:48   t13ae31
      *AKI01100: Central Bestandsdaten täglich aktualisieren
      *
      *   Rev 2.2   Aug 18 2011 13:52:32   t13ae31
      *AKI00886: Host - neues Design Kundenaktualisierung
      *Anpassun
      *g CKV
      *
      *   Rev 2.1   Jun 21 1999 11:11:22   T13EX50
      *Anpassen an die neue Satzstruktur 
      *
      *   Rev 2.0   Apr 15 1999 11:31:24   T13EX50
      *Produktionsübergabe - CKV
      *
      *   Rev 1.1   Mar 25 1999 10:37:24   T13EX50
      *Erste Version 
      *
      *   Rev 1.0   25 Aug 1998 09:22:48   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *    COPY-MEMBER ZU DEN AUFBEREITUNGSPROGRAMMEN                  *
      *   - Ausgabe CKV VERSPERSOND - VERSTARVERSOBJEKT -VERSOBJEKT    *
      ******************************************************************
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                         !NAME     !VERS.*
      *AUFTRAG !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *01.02.99!NEUERSTELLUNG                          !HERWEG   ! 01  *
      *AKI00022!                                       !         !     *
      *        !                                       !         !     *
      *31.05.99!EINBAU WHR-KZ                          !HERWEG   ! 1.1 *
      *FKI00042!                                       !         !     *
      *        !                                       !         !     *
130711*AKI00886! Umstrukt. f. neue Kd.akt nur noch     ! Hübner  !     *
      *        ! Versichertertarif wird bearbeitet     !         !     *
      ******************************************************************
         01 DRKI0606-SATZ.
      *      * ALLGEMEINE DATEN
             10  VT-GES                      PIC 9(02).
             10  VT-VNR                      PIC 9(15).
130711       10  VT-LFDNR                    PIC 9(02).

      *      * VERSICHERTERTARIF
             10  VT-VERSSUMME                PIC 9(09)V99.
             10  VT-VERSSUMME-KZ             PIC X(01).
             10  VT-VERSSUMME-WHR            PIC X(01).
             10  VT-VERSSUMME-WHR-KZ         PIC X(01).
             10  VT-PRODUKTID                PIC 9(02).
             10  VT-PRODUKTID-KZ             PIC X(01).
             10  VT-SUBTYPE                  PIC X(01).
130711       10  VT-AKTUAL-KZ                PIC X(01).
      *
      *        I = INSERT
      *        U = UPDATE
      *        D = DELETE
      *****************************************************************
      *        E N D E  Copy-Buch   DRKI0606                          *
      *****************************************************************
