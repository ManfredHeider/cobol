      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DWAT0019.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DWAT0019.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:48   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 09:31:46   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: DWAT0019      PERSDAT-ZUGRIFFE                * 00020000
      *    --------------------------------------------------------   * 00030000
      *    VERSION   1    30.08.94 17:01:21
      *    --------------------------------------------------------   * 00050000
      *    ERSTELLT: W. SCHÖLLHAMMER (INTEGRATA AG)                   * 00060000
      *    DATUM:    01.04.1993                                       * 00070000
      *    STAND:    14.04.1993                                       * 00080000
      ***************************************************************** 00090000
      *    GEAENDERT: WIECHERS - AFRZ0013 - VERSION 01 - 30.08.94     * 00091001
      *               UMSTELLUNG KUS / RACF                           * 00092001
      ***************************************************************** 00094001
      *                                                                 00100000
      *    SCHNITTSTELLE FUER ZUGRIFFE AUF PERSDAT (ANWENDERDATEI)      00110000
      *                                                                 00120000
      *    ABHÄNGIG VON ZI-DWAT0019-PD-SERVICE                          00130000
      *    MUESSEN BEIM AUFRUF ÜBERGEBEN WERDEN:                        00140000
      *       ------------------------+-------+-------+-------+-------+ 00150000
      *       ZI-DWAT0019-PD-         !SERVICE!       !       !       ! 00160000
      *       ZIO-DWAT0019-PD-        !       !  KEY  ! DATA  !       ! 00161000
      *       ------------------------+-------+-------+-------+-------+ 00170000
      *     * PD-SERVICE-STARTBR-EQUAL!   X   !   X   !       !       ! 00180000
      *       PD-SERVICE-STARTBR-GTEQ !   X   !   X   !       !       ! 00190000
      *       PD-SERVICE-ENDBR        !   X   !       !       !       ! 00200000
      *       PD-SERVICE-READNEXT     !   X   !   X   !       !       ! 00210000
      *     * PD-SERVICE-READPREV     !   X   !   X   !       !       ! 00220000
      *       PD-SERVICE-READ         !   X   !   X   !       !       ! 00230000
      *     * PD-SERVICE-READUPDATE   !   X   !   X   !       !       ! 00240000
      *     * PD-SERVICE-REWRITE      !   X   !       !   X   !       ! 00250000
      *     * PD-SERVICE-WRITE        !   X   !   X   !   X   !       ! 00260000
      *       ------------------------+-------+-------+-------+-------+ 00270000
      *                                                                 00280000
      *    ABHÄNGIG VON ZI-DWAT0019-PD-SERVICE                          00290000
      *    WERDEN VOM SERVICE ZURÜCKGELIEFERT:                          00300000
      *       ------------------------+-------+-------+-------+-------+ 00310000
      *       ZI-DWAT0019-PD-         !  RC   !       !       !       ! 00320000
      *       ZIO-DWAT0019-PD-        !       !  KEY  ! DATA  !       ! 00321000
      *       ------------------------+-------+-------+-------+-------+ 00330000
      *     * PD-SERVICE-STARTBR-EQUAL!   X   !       !       !       ! 00340000
      *       PD-SERVICE-STARTBR-GTEQ !   X   !       !       !       ! 00350000
      *       PD-SERVICE-ENDBR        !   X   !       !       !       ! 00360000
      *       PD-SERVICE-READNEXT     !   X   !   X   !   X   !       ! 00370000
      *     * PD-SERVICE-READPREV     !   X   !   X   !   X   !       ! 00380000
      *       PD-SERVICE-READ         !   X   !       !   X   !       ! 00390000
      *     * PD-SERVICE-READUPDATE   !   X   !       !   X   !       ! 00400000
      *     * PD-SERVICE-REWRITE      !   X   !       !       !       ! 00410000
      *     * PD-SERVICE-WRITE        !   X   !       !       !       ! 00420000
      *       ------------------------+-------+-------+-------+-------+ 00430000
      *                                                                 00440000
      *    ANMERKUNGEN:                                                 00450000
      *        *  ZUR ZEIT NICHT UNTERSTÜTZT                            00460000
      *                                                                 00470000
      *    MÖGLICHE RETURN-WERTE:                                       00480000
      *       ------------------------+----+----+----+----+----+        00490000
      *       PD-RC-                  ! OK !NOT !END !DUP !DUP !        00500000
      *                               !    !FND !FILE!REC !KEY !        00510000
      *       ------------------------+----+----+----+----+----+        00520000
      *     * PD-SERVICE-STARTBR-EQUAL! X  ! X  !    !    !    !        00530000
      *       PD-SERVICE-STARTBR-GTEQ ! X  ! X  !    !    !    !        00540000
      *       PD-SERVICE-ENDBR        ! X  !    !    !    !    !        00550000
      *       PD-SERVICE-READNEXT     ! X  ! X  ! X  !    ! X  !        00560000
      *     * PD-SERVICE-READPREV     ! X  ! X  ! X  !    !    !        00570000
      *       PD-SERVICE-READ         ! X  ! X  !    !    !    !        00580000
      *     * PD-SERVICE-READUPDATE   ! X  ! X  !    !    !    !        00590000
      *     * PD-SERVICE-REWRITE      ! X  !    !    !    !    !        00600000
      *     * PD-SERVICE-WRITE        ! X  !    !    ! X  !    !        00610000
      *       ------------------------+----+----+----+----+----!        00620000
      *                                                                 00630000
      * -------------------------------------------------------------   00640000
       01  ZI-DWAT0019-INPUT.                                           00650000
           05  ZI-DWAT0019-PD-SERVICE         PIC  9(002).              00660000
      *         88  PD-SERVICE-STARTBR-EQUAL                VALUE 1.    00670000
               88  PD-SERVICE-STARTBR-GTEQ                 VALUE 2.     00680000
               88  PD-SERVICE-ENDBR                        VALUE 3.     00690000
               88  PD-SERVICE-READNEXT                     VALUE 4.     00700000
      *         88  PD-SERVICE-READPREV                     VALUE 5.    00710000
               88  PD-SERVICE-READ                         VALUE 6.     00720000
      *         88  PD-SERVICE-READUPDATE                   VALUE 7.    00730000
      *         88  PD-SERVICE-REWRITE                      VALUE 8.    00740000
      *         88  PD-SERVICE-WRITE                        VALUE 9.    00750000
                                                                        00780000
       01  ZO-DWAT0019-OUTPUT.                                          00790000
           05  ZO-DWAT0019-PD-RC              PIC  9(001).              00800000
               88  PD-RC-OK                                VALUE 0.     00810000
               88  PD-RC-NOTFND                            VALUE 1.     00820000
               88  PD-RC-ENDFILE                           VALUE 2.     00830000
      *         88  PD-RC-DUPREC                            VALUE 3.    00840000
               88  PD-RC-DUPKEY                            VALUE 4.     00850000
                                                                        00880000
       01  ZIO-DWAT0019-INOUT.                                          00881000
300894*    05  ZIO-DWAT0019-PD-KEY            PIC  X(010).              00888001
300894*    05  ZIO-DWAT0019-PD-DATA           PIC  X(120).              00889001
300894     05  ZIO-DWAT0019-PD-KEY            PIC  X(013).              00889101
300894     05  ZIO-DWAT0019-PD-DATA           PIC  X(050).              00889201
      *                                                                 00889300
      ******* ENDE COPY-MEMBER DWAT0019 (PERSDAT-ZUGRIFFE)    ********* 00890000
CCC009*    EJECT                                                        00900000
