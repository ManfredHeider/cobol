      * T13EX33   Stand: 31/07/98    13:10:07
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0355.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRKI0355.CPv  $
      *
      *   Rev 2.0   Mar 23 1999 15:52:36   T13EX33
      *aki00019 Produktionsübergabe
      *
      *   Rev 1.0   14 Jul 1998 17:01:48   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *              #####################                             *
      *     ########    D R K I 0 3 5 5    ########                    *
      *              #####################                             *
      *                                                                *
      *  COPY-MEMBER FUER AUFBEREITUNGSPROGRAMM AML VERTRAGSDATEN      *
      *       LEBENTEILTYP - provisionsdaten                           *
      *                                                                *
      *    SATZLÄNGE   50                                              *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                         !NAME     !VERS.*
      *AUFTRAG !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *16.07.98!NEUERSTELLUNG                          !HUEBNER  ! 01  *
      *KI-NEU  !                                       !         !     *
      *        !                                       !         !     *
      *        !                                       !         !     *
      *        !                                       !         !     *
      ******************************************************************
       01  DRKI0355.

           02  AML-PROVISION.
           05  AML-GES                    PIC  9(02).
           05  AML-VNR                    PIC  9(15).

           05  AML-TEILTYPNR              PIC  9(02).
           05  AML-PROVVON                PIC  X(10).
           05  AML-PROVVON-KZ             PIC  X(01).
      *        J WENN PROVVON GEAENDERT
           05  AML-PROVBIS                PIC  X(10).
           05  AML-PROVBIS-KZ             PIC  X(01).
      *        J WENN PROVBIS GEAENDERT
           05  AML-ABVERM                 PIC  9(07).
           05  AML-ABVERM-KZ              PIC  X(01).
      *        J WENN ABVERM GEAENDERT
           05  AML-AKTUAL-KZ              PIC  X(01).
      ******************************************************************
