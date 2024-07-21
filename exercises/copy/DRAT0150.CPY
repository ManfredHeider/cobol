      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0150.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAT0150.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:28   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   27 Aug 1997 17:45:34   T13EX23
      *AFPA0022 - ANPASSUNG RAHMEN\DRIVER
      *
      *   Rev 1.0   Feb 19 1997 12:22:48   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: Übergabebereich  (upat0018)                   *
      *                 für K-K Status                                *
      *    --------------------------------------------------------   *
      *    ERSTELLT: J. Opritz       (INTEGRATA AG)                   *
      *    DATUM:    18.01.1993                                       *
      *****************************************************************
      *    VERSION   1    10.01.95 16:25:25
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * afpa0022!14.08.97! 01 ! NEU                       ! Opritz   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *                               ! CALL! OK ! ! NOT FOUND !
      *                               !     !    ! !           !
      *      PI-drat0150-KTY          !  X  !
      *      PI-drat0150-KNR          !  X  !
      *      ---------------------------------------------------
      *      PO-drat0150-RETCODE      !     ! 0  ! !    1      !
      *      PO-drat0150-text               ! X  ! !           !
      *

       01   PI-DRAT0150.
         05  PI-DRAT0150-KTY                 PIC S9(004) COMP.
         05  PI-DRAT0150-KNR                 PIC S9(004) COMP.

       01   PO-DRAT0150.
         05  PO-DRAT0150-RC                  PIC  X(003).
               88 PO-DRAT0150-RC-OK                        VALUE 'OK '.
               88 PO-DRAT0150-RC-NOT-FOUND                 VALUE 'NFD'.
         05  PO-DRAT0150-KTY-KNR-MELDUNG.
             10  PO-DRAT0150-ATTRIBUT        PIC  X(001).
             10  PO-DRAT0150-TEXT            PIC  X(079).


      ******** ENDE COPY-MEMBER Übergabebereich        (DRAT0014) *****
