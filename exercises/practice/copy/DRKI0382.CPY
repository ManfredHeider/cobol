      * T13EX39   Stand: 21/07/98    21:34:33
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0382.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI0382.CPv  $
      *
      *   Rev 2.1   19 Oct 2006 09:08:04   t13ae31
      *AKI00615: Einführung Pflegerentenversicherung
      *
      *   Rev 2.0   Mar 23 1999 15:52:52   T13EX33
      *aki00019 Produktionsübergabe
      *
      *   Rev 1.0   14 Jul 1998 17:03:40   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *              #####################                             *
      *     ########    D R K I 0 3 8 2    ########                    *
      *              #####################                             *
      *                                                                *
      *  COPY-MEMBER FUER AUFBEREITUNGSPROGRAMM AML VERTRAGSDATEN      *
      *       VERSICHERTERTARIF                                        *
      *                                                                *
      *    SATZLÄNGE   22                                              *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                         !NAME     !VERS.*
      *AUFTRAG !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *21.07.98!NEUERSTELLUNG                          !HUEBNER  ! 01  *
      *KI-NEU  !                                       !         !     *
      *        !                                       !         !     *
      *30.09.06! Änd. Produkt-id auf 3 Stellen         !Huebner  !     *
      *        !                                       !         !     *
      ******************************************************************
       01  DRKI0382.

           02  AML-VERSICHERTERTARIF.
           05  AML-GES                    PIC  9(02).
           05  AML-VNR                    PIC  9(15).

      ***    05  AML-PRODUKT-ID             PIC  9(02).
300906     05  AML-PRODUKT-ID             PIC  9(03).
           05  AML-AEND-PRODUKT-KZ        PIC  X(01).
      *        J WENN PRODUKT GEäNDERT
           05  AML-AKTUAL-KZ              PIC  X(01).
      ******************************************************************
