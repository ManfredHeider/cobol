      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0014.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAT0014.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:04   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 08:36:12   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: Übergabebereich  (DRAT0014)                   * 00020000
      *                 für Domänenprüfung Schlüsselfeld              * 00030000
      *    --------------------------------------------------------   *
      *    ERSTELLT: M. GIESE        (INTEGRATA AG)                   *
      *    DATUM:    18.01.1993                                       *
      *****************************************************************
      *    VERSION   1    10.01.95 16:25:25
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !18.01.93! 01 ! NEU                       ! GIESE    *
      * AFVB0163!10.01.95! 02 ! ERWEITERUNG UM BEZ        ! GIESE    *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
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
      *      PI-UPVB0008-KTY          !  X  !
      *      PI-UPVB0008-KNR          !  X  !
      *      ---------------------------------------------------
      *      PO-UPVB0008-RETCODE      !     ! 0  ! !    1      !
      *      PO-UPVB0008-KNR-BEZ-KURZ !     ! X  ! !           !
      *      PO-UPVB0008-KNR-BEZ            ! X  ! !           !
      *      PO-UPVB0008-KNR-BEZ10          ! X  ! !           !
      *

       01   PI-UPVB0008.
         05  PI-UPVB0008-KTY           PIC S9(04) COMP.
         05  PI-UPVB0008-KNR           PIC S9(04) COMP.

       01   PO-UPVB0008.
         05  PO-UPVB0008-RETCODE       PIC  X(01).
         05  PO-UPVB0008-KNR-BEZ-KURZ  PIC  X(04).
100195   05  PO-UPVB0008-KNR-BEZ       PIC  X(40).
100195   05  PO-UPVB0008-KNR-BEZ10     PIC  X(10).

                                                                        01590000
      ******** ENDE COPY-MEMBER Übergabebereich        (DRAT0014) ***** 01600000
