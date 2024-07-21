      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DWAT0018.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DWAT0018.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:48   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 09:31:30   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: DWAT0018      TRANSDAT-ZUGRIFFE               * 00020001
      *    --------------------------------------------------------   * 00030000
      *    VERSION   001                                              * 00040000
      *    --------------------------------------------------------   * 00050000
      *    ERSTELLT: W. SCHÖLLHAMMER (INTEGRATA AG)                   * 00060000
      *    DATUM:    01.04.1993                                       * 00070000
      *    STAND:    20.04.1993                                       * 00080007
      ***************************************************************** 00090000
      *                                                                 00100000
      *    SCHNITTSTELLE FUER ZUGRIFFE AUF TRANSDAT (ANWENDUNGSDATEI)   00110001
      *                                                                 00120000
      *    ABHÄNGIG VON ZI-DWAT0018-TD-SERVICE                          00130001
      *    MUESSEN BEIM AUFRUF ÜBERGEBEN WERDEN:                        00140000
      *       ------------------------+-------+-------+-------+-------+ 00150000
      *       ZI-DWAT0018-TD-         !SERVICE!       !       !       ! 00160006
      *       ZIO-DWAT0018-TD-        !       !  KEY  ! DATA  !       ! 00161006
      *       ------------------------+-------+-------+-------+-------+ 00170000
      *     * TD-SERVICE-STARTBR-EQUAL!   X   !   X   !       !       ! 00180001
      *     * TD-SERVICE-STARTBR-GTEQ !   X   !   X   !       !       ! 00190001
      *     * TD-SERVICE-ENDBR        !   X   !       !       !       ! 00200001
      *     * TD-SERVICE-READNEXT     !   X   !   X   !       !       ! 00210005
      *     * TD-SERVICE-READPREV     !   X   !   X   !       !       ! 00220005
      *       TD-SERVICE-READ         !   X   !   X   !       !       ! 00230001
      *     * TD-SERVICE-READUPDATE   !   X   !   X   !       !       ! 00240001
      *     * TD-SERVICE-REWRITE      !   X   !       !   X   !       ! 00250001
      *     * TD-SERVICE-WRITE        !   X   !   X   !   X   !       ! 00260001
      *       ------------------------+-------+-------+-------+-------+ 00270000
      *                                                                 00280000
      *    ABHÄNGIG VON ZI-DWAT0018-TD-SERVICE                          00290001
      *    WERDEN VOM SERVICE ZURÜCKGELIEFERT:                          00300000
      *       ------------------------+-------+-------+-------+-------+ 00310000
      *       ZI-DWAT0018-TD-         !  RC   !       !       !       ! 00320006
      *       ZIO-DWAT0018-TD-        !       !  KEY  ! DATA  !       ! 00321006
      *       ------------------------+-------+-------+-------+-------+ 00330000
      *     * TD-SERVICE-STARTBR-EQUAL!   X   !       !       !       ! 00340001
      *     * TD-SERVICE-STARTBR-GTEQ !   X   !       !       !       ! 00350001
      *     * TD-SERVICE-ENDBR        !   X   !       !       !       ! 00360001
      *     * TD-SERVICE-READNEXT     !   X   !   X   !   X   !       ! 00370001
      *     * TD-SERVICE-READPREV     !   X   !   X   !   X   !       ! 00380001
      *       TD-SERVICE-READ         !   X   !       !   X   !       ! 00390001
      *     * TD-SERVICE-READUPDATE   !   X   !       !   X   !       ! 00400001
      *     * TD-SERVICE-REWRITE      !   X   !       !       !       ! 00410001
      *     * TD-SERVICE-WRITE        !   X   !       !       !       ! 00420001
      *       ------------------------+-------+-------+-------+-------+ 00430000
      *                                                                 00440000
      *    ANMERKUNGEN:                                                 00450000
      *        *  ZUR ZEIT NICHT UNTERSTÜTZT                            00460000
      *                                                                 00470000
      *    MÖGLICHE RETURN-WERTE:                                       00611003
      *       ------------------------+----+----+----+----+----+        00612003
      *       PD-RC-                  ! OK !NOT !END !DUP !DUP !        00613003
      *                               !    !FND !FILE!REC !KEY !        00614003
      *       ------------------------+----+----+----+----+----+        00615003
      *     * PD-SERVICE-STARTBR-EQUAL! X  ! X  !    !    !    !        00616003
      *     * PD-SERVICE-STARTBR-GTEQ ! X  ! X  !    !    !    !        00617003
      *     * PD-SERVICE-ENDBR        ! X  !    !    !    !    !        00618003
      *     * PD-SERVICE-READNEXT     ! X  ! X  ! X  !    ! X  !        00619003
      *     * PD-SERVICE-READPREV     ! X  ! X  ! X  !    !    !        00619103
      *       PD-SERVICE-READ         ! X  ! X  !    !    !    !        00619203
      *     * PD-SERVICE-READUPDATE   ! X  ! X  !    !    !    !        00619303
      *     * PD-SERVICE-REWRITE      ! X  !    !    !    !    !        00619403
      *     * PD-SERVICE-WRITE        ! X  !    !    ! X  !    !        00619503
      *       ------------------------+----+----+----+----+----!        00619603
      *                                                                 00620000
      * -------------------------------------------------------------   00630000
       01  ZI-DWAT0018-INPUT.                                           00640000
           05  ZI-DWAT0018-TD-SERVICE         PIC  9(002).              00650001
      *         88  TD-SERVICE-STARTBR-EQUAL                VALUE 1.    00660001
      *         88  TD-SERVICE-STARTBR-GTEQ                 VALUE 2.    00670001
      *         88  TD-SERVICE-ENDBR                        VALUE 3.    00680001
      *         88  TD-SERVICE-READNEXT                     VALUE 4.    00690001
      *         88  TD-SERVICE-READPREV                     VALUE 5.    00700001
               88  TD-SERVICE-READ                         VALUE 6.     00710001
      *         88  TD-SERVICE-READUPDATE                   VALUE 7.    00720001
      *         88  TD-SERVICE-REWRITE                      VALUE 8.    00730001
      *         88  TD-SERVICE-WRITE                        VALUE 9.    00740001
                                                                        00770000
       01  ZO-DWAT0018-OUTPUT.                                          00780000
           05  ZO-DWAT0018-TD-RC              PIC  9(001).              00790001
               88  TD-RC-OK                                VALUE 0.     00800001
               88  TD-RC-NOTFND                            VALUE 1.     00810001
               88  TD-RC-ENDFILE                           VALUE 2.     00820001
      *         88  TD-RC-DUPREC                            VALUE 3.    00830001
                                                                        00860005
       01  ZIO-DWAT0018-INOUT.                                          00861005
           05  ZIO-DWAT0018-TD-KEY            PIC  X(006).              00869307
           05  ZIO-DWAT0018-TD-DATA           PIC  X(200).              00869405
                                                                        00869505
      *                                                                 00870405
      ******* ENDE COPY-MEMBER DWAT0018 (TRANSDAT-ZUGRIFFE)   ********* 00871001
CCC009*    EJECT                                                        00880000
