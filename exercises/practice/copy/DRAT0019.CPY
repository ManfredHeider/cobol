      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0019.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAT0019.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:04   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.5   13 Nov 1998 16:53:32   T13EX26
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.4   Mar 20 1997 16:56:48   T13EX16
      *DRIVER-ANPASSUNG DAISY
      *
      *   Rev 1.3   Jun 24 1996 13:02:48   T13EX16
      *AFPA0001: doppelte Definition PA01 behoben
      *
      *   Rev 1.2   Mar 26 1996 17:56:08   T13EX11
      *AFVB0226  neue Y-Transaktionen
      *
      *   Rev 1.1   Nov 15 1995 16:30:42   T13EX11
      *AFVB0226 Neue Y-Transaktionen u. a. für VB36
      *         und VB91
      *
      *   Rev 1.0   Nov 04 1995 08:37:16   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: TRANS-CODES FUER VORSCHALTPROGRAMM (DRAT0019) *
      *    --------------------------------------------------------   *
      *    ERSTELLT: W. SCHÖLLHAMMER (INTEGRATA AG)                   *
      *    DATUM:    08.02.1993                                       *
      *****************************************************************
      *    VERSION   3    17.11.95 14:23:15
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !23.04.93! 01 ! NEU                       ! SCHÖLLH. *
      *         !15.06.93! 02 ! TRANS 'VB35', 'Y026' NEU  ! SCHÖLLH. *
      * AFVB0102!18.04.94!    ! TRANS 'VB13', 'Y017' NEU  ! SCHÖLLH. *
      *         !22.04.94! 03 ! PROD.UEBERGABE            ! SCHÖLLH. *
      * AFOP0030!14.09.94!    ! TRANS 'VB13', 'Y017' NEU  ! SCHÖLLH. *
      * AFVB0163!04.11.94!    ! SF14, ZA08 UND SF20 - SF25!          *
      *         !10.01.95! 04 ! PRODUKTIONSÜBERGABE       ! GIESE    *
      * OHNE    !07.03.95!    ! NEUE TRANS                ! GIESE    *
      * AFVB0226!24.07.95!    ! NEUE TRANS 'VB91'         ! SCHÖLLH. *
      * AFVB0237!12.09.95!    ! NEUE TRANS 'VB36'         ! SCHMIDT  *
      *         !07.11.95! 03 ! PROD.UEBERGABE            ! SCHÖLLH. *
      * AFVB0226!02.02.96!    ! Viele neue TRANS für      ! Schöllh. *
      *         !        !    !  Finanzstatus             !          *
      * ???     !15.02.96!    ! neue y-trans für pa01/pa02! Giese    *
      * AFPA0001!21.06.96!    ! PA01/PA02 doppelt definiert Auerbach *
      *         !        !    ! eins entfernt             !          *
      * AFPA0002!01.10.96! 04 ! PW12, PA11, PA12, PA14 NEU! AUERBACH *
      * AFPA0002!28.10.96!    ! NEUE TRANS 'PA15'         ! SCHäFER  *
      *         !        !    !                           !          *
      * AFPA0005!......97!    ! SF31, SF32,XA11, XA12     !BOHLINGER *
      * AFPA0007!03.03.97!    ! XA13                      !BOHLINGER *
      * AFPA0013!30.05.97!    ! PV11, ZA11, PV12, ZA12    !BOHLINGER *
      *         !28.10.97!    ! ZV11, ZV12, PG11, PG12    !MUND      *
      *         !28.10.97!    ! PG13, PG14, PG15, PG16    !MUND      *
      *         !28.10.97!    ! PG18, SF41, SF42, PG17    !MUND      *
      *         !10.11.97!    ! PG20                      !MUND      *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    ZU JEDEM TRANSAKTIONSCODE DER NEUANWENDUNGEN GIBT ES EINEN
      *    ZWILLINGS-TRANSAKTIONSCODE, DEM DER GLEICHE DB2-PLAN
      *    ZUGEORDNET IST, MIT DEM ABER DAS VORSCHALTPROGRAMM AUFGE-
      *    RUFEN WIRD (MIT START TRANSID), UM DEN PLAN ZU WECHSELN.
      *
      *    ACHTUNG! BEI ERGAENZUNGEN ANPASSUNGEN NOTWENDIG:
      *             - OCCURS-WERT
      *
       01  T-TRANSSCHALT-TAB.
      *        INHALT JEWEILS:
      *            TRANS-CODE - NORMAL
      *            TRANS-CODE - FUER VORSCHALTPROGRAMM (Y...)
           05  FILLER.
               10  FILLER                PIC  X(004) VALUE 'VB21'.
               10  FILLER                PIC  X(004) VALUE 'Y001'.
           05  FILLER.
               10  FILLER                PIC  X(004) VALUE 'VB22'.
               10  FILLER                PIC  X(004) VALUE 'Y002'.
           05  FILLER.
               10  FILLER                PIC  X(004) VALUE 'VB28'.
               10  FILLER                PIC  X(004) VALUE 'Y003'.
           05  FILLER.
               10  FILLER                PIC  X(004) VALUE 'XA01'.
               10  FILLER                PIC  X(004) VALUE 'Y004'.
           05  FILLER.
               10  FILLER                PIC  X(004) VALUE 'XA02'.
               10  FILLER                PIC  X(004) VALUE 'Y005'.
           05  FILLER.
               10  FILLER                PIC  X(004) VALUE 'ZA02'.
               10  FILLER                PIC  X(004) VALUE 'Y006'.
           05  FILLER.
               10  FILLER                PIC  X(004) VALUE 'ZA03'.
               10  FILLER                PIC  X(004) VALUE 'Y007'.
           05  FILLER.
               10  FILLER                PIC  X(004) VALUE 'SF11'.
               10  FILLER                PIC  X(004) VALUE 'Y008'.
           05  FILLER.
               10  FILLER                PIC  X(004) VALUE 'SF12'.
               10  FILLER                PIC  X(004) VALUE 'Y009'.
           05  FILLER.
               10  FILLER                PIC  X(004) VALUE 'SF13'.
               10  FILLER                PIC  X(004) VALUE 'Y010'.
    ***10
           05  FILLER.
               10  FILLER                PIC  X(004) VALUE 'VB30'.
               10  FILLER                PIC  X(004) VALUE 'Y011'.
           05  FILLER.
               10  FILLER                PIC  X(004) VALUE 'VB31'.
               10  FILLER                PIC  X(004) VALUE 'Y012'.
           05  FILLER.
               10  FILLER                PIC  X(004) VALUE 'VB34'.
               10  FILLER                PIC  X(004) VALUE 'Y013'.
           05  FILLER.
               10  FILLER                PIC  X(004) VALUE 'VB33'.
               10  FILLER                PIC  X(004) VALUE 'Y014'.
           05  FILLER.
               10  FILLER                PIC  X(004) VALUE 'VB12'.
               10  FILLER                PIC  X(004) VALUE 'Y015'.
           05  FILLER.
               10  FILLER                PIC  X(004) VALUE 'XA03'.
               10  FILLER                PIC  X(004) VALUE 'Y016'.
           05  FILLER.
               10  FILLER                PIC  X(004) VALUE 'VB11'.
               10  FILLER                PIC  X(004) VALUE 'Y017'.
180494     05  FILLER.
180494         10  FILLER                PIC  X(004) VALUE 'VB13'.
180494         10  FILLER                PIC  X(004) VALUE 'Y017'.
           05  FILLER.
               10  FILLER                PIC  X(004) VALUE 'VB23'.
               10  FILLER                PIC  X(004) VALUE 'Y018'.
           05  FILLER.
               10  FILLER                PIC  X(004) VALUE 'XA04'.
               10  FILLER                PIC  X(004) VALUE 'Y019'.
    ***20
           05  FILLER.
               10  FILLER                PIC  X(004) VALUE 'VB24'.
               10  FILLER                PIC  X(004) VALUE 'Y020'.
           05  FILLER.
               10  FILLER                PIC  X(004) VALUE 'VB25'.
               10  FILLER                PIC  X(004) VALUE 'Y021'.
           05  FILLER.
               10  FILLER                PIC  X(004) VALUE 'VB29'.
               10  FILLER                PIC  X(004) VALUE 'Y022'.
           05  FILLER.
               10  FILLER                PIC  X(004) VALUE 'XA05'.
               10  FILLER                PIC  X(004) VALUE 'Y023'.
           05  FILLER.
               10  FILLER                PIC  X(004) VALUE 'VB32'.
               10  FILLER                PIC  X(004) VALUE 'Y024'.
    ***25
           05  FILLER.
               10  FILLER                PIC  X(004) VALUE 'ZA04'.
               10  FILLER                PIC  X(004) VALUE 'Y025'.
           05  FILLER.
               10  FILLER                PIC  X(004) VALUE 'VB10'.
               10  FILLER                PIC  X(004) VALUE 'Y025'.
           05  FILLER.
               10  FILLER                PIC  X(004) VALUE 'VB20'.
               10  FILLER                PIC  X(004) VALUE 'Y025'.
           05  FILLER.
               10  FILLER                PIC  X(004) VALUE 'VB40'.
               10  FILLER                PIC  X(004) VALUE 'Y025'.
           05  FILLER.
               10  FILLER                PIC  X(004) VALUE 'DI10'.
               10  FILLER                PIC  X(004) VALUE 'Y025'.
    ***30
           05  FILLER.
               10  FILLER                PIC  X(004) VALUE 'SD10'.
               10  FILLER                PIC  X(004) VALUE 'Y025'.
           05  FILLER.
               10  FILLER                PIC  X(004) VALUE 'VS10'.
               10  FILLER                PIC  X(004) VALUE 'Y025'.
           05  FILLER.
               10  FILLER                PIC  X(004) VALUE 'SF10'.
               10  FILLER                PIC  X(004) VALUE 'Y025'.
           05  FILLER.
               10  FILLER                PIC  X(004) VALUE 'VB35'.
               10  FILLER                PIC  X(004) VALUE 'Y026'.
140994     05  FILLER.
140994         10  FILLER                PIC  X(004) VALUE 'XO01'.
140994         10  FILLER                PIC  X(004) VALUE 'Y027'.
    ***35
140994     05  FILLER.
140994         10  FILLER                PIC  X(004) VALUE 'XO02'.
140994         10  FILLER                PIC  X(004) VALUE 'Y028'.
041194     05  FILLER.
041194         10  FILLER                PIC  X(004) VALUE 'SF20'.
041194         10  FILLER                PIC  X(004) VALUE 'Y029'.
041194     05  FILLER.
041194         10  FILLER                PIC  X(004) VALUE 'SF21'.
041194         10  FILLER                PIC  X(004) VALUE 'Y030'.
041194     05  FILLER.
041194         10  FILLER                PIC  X(004) VALUE 'SF22'.
041194         10  FILLER                PIC  X(004) VALUE 'Y031'.
041194     05  FILLER.
041194         10  FILLER                PIC  X(004) VALUE 'SF23'.
041194         10  FILLER                PIC  X(004) VALUE 'Y032'.
    ***40
041194     05  FILLER.
041194         10  FILLER                PIC  X(004) VALUE 'SF24'.
041194         10  FILLER                PIC  X(004) VALUE 'Y033'.
041194     05  FILLER.
041194         10  FILLER                PIC  X(004) VALUE 'SF25'.
041194         10  FILLER                PIC  X(004) VALUE 'Y034'.
041194     05  FILLER.
041194         10  FILLER                PIC  X(004) VALUE 'SF14'.
041194         10  FILLER                PIC  X(004) VALUE 'Y035'.
041194     05  FILLER.
041194         10  FILLER                PIC  X(004) VALUE 'ZA08'.
041194         10  FILLER                PIC  X(004) VALUE 'Y036'.
070395     05  FILLER.
070395         10  FILLER                PIC  X(004) VALUE 'VB38'.
070395         10  FILLER                PIC  X(004) VALUE 'Y037'.
    ***45
070395     05  FILLER.
070395         10  FILLER                PIC  X(004) VALUE 'VB37'.
070395         10  FILLER                PIC  X(004) VALUE 'Y038'.
070395     05  FILLER.
070395         10  FILLER                PIC  X(004) VALUE 'VB59'.
070395         10  FILLER                PIC  X(004) VALUE 'Y039'.
070395     05  FILLER.
070395         10  FILLER                PIC  X(004) VALUE 'XA07'.
070395         10  FILLER                PIC  X(004) VALUE 'Y040'.
291295*     05  FILLER.
291295*         10  FILLER                PIC  X(004) VALUE 'XA06'.
291295*         10  FILLER                PIC  X(004) VALUE 'Y041'.
210696*     05  FILLER.
210696*         10  FILLER                PIC  X(004) VALUE 'PA01'.
210696*         10  FILLER                PIC  X(004) VALUE 'Y041'.
210696*     05  FILLER.
210696*         10  FILLER                PIC  X(004) VALUE 'PA02'.
210696*         10  FILLER                PIC  X(004) VALUE 'Y041'.
070395     05  FILLER.
070395         10  FILLER                PIC  X(004) VALUE 'AN30'.
070395         10  FILLER                PIC  X(004) VALUE 'Y042'.
240795     05  FILLER.
240795         10  FILLER                PIC  X(004) VALUE 'VB91'.
240795         10  FILLER                PIC  X(004) VALUE 'Y043'.
    ***50
120995     05  FILLER.
120995         10  FILLER                PIC  X(004) VALUE 'VB36'.
120995         10  FILLER                PIC  X(004) VALUE 'Y044'.
020296     05  FILLER.
020296         10  FILLER                PIC  X(004) VALUE 'VB62'.
020296         10  FILLER                PIC  X(004) VALUE 'Y045'.
020296     05  FILLER.
020296         10  FILLER                PIC  X(004) VALUE 'VB64'.
020296         10  FILLER                PIC  X(004) VALUE 'Y047'.
020296     05  FILLER.
020296         10  FILLER                PIC  X(004) VALUE 'VB82'.
020296         10  FILLER                PIC  X(004) VALUE 'Y048'.
020296     05  FILLER.
020296         10  FILLER                PIC  X(004) VALUE 'VB83'.
020296         10  FILLER                PIC  X(004) VALUE 'Y050'.
    ***55
020296     05  FILLER.
020296         10  FILLER                PIC  X(004) VALUE 'VB81'.
020296         10  FILLER                PIC  X(004) VALUE 'Y052'.
020296     05  FILLER.
020296         10  FILLER                PIC  X(004) VALUE 'VB84'.
020296         10  FILLER                PIC  X(004) VALUE 'Y053'.
020296     05  FILLER.
020296         10  FILLER                PIC  X(004) VALUE 'VB71'.
020296         10  FILLER                PIC  X(004) VALUE 'Y054'.
020296     05  FILLER.
020296         10  FILLER                PIC  X(004) VALUE 'VB79'.
020296         10  FILLER                PIC  X(004) VALUE 'Y056'.
020296     05  FILLER.
020296         10  FILLER                PIC  X(004) VALUE 'XA08'.
020296         10  FILLER                PIC  X(004) VALUE 'Y046'.
    ***60
020296     05  FILLER.
020296         10  FILLER                PIC  X(004) VALUE 'VB61'.
020296         10  FILLER                PIC  X(004) VALUE 'Y057'.
020296     05  FILLER.
020296         10  FILLER                PIC  X(004) VALUE 'XA09'.
020296         10  FILLER                PIC  X(004) VALUE 'Y049'.
020296     05  FILLER.
020296         10  FILLER                PIC  X(004) VALUE 'VB65'.
020296         10  FILLER                PIC  X(004) VALUE 'Y058'.
020296     05  FILLER.
020296         10  FILLER                PIC  X(004) VALUE 'XA10'.
020296         10  FILLER                PIC  X(004) VALUE 'Y055'.
020296     05  FILLER.
020296         10  FILLER                PIC  X(004) VALUE 'VB63'.
020296         10  FILLER                PIC  X(004) VALUE 'Y059'.
    ***65
020296     05  FILLER.
020296         10  FILLER                PIC  X(004) VALUE 'VB60'.
020296         10  FILLER                PIC  X(004) VALUE 'Y025'.
291295     05  FILLER.
291295         10  FILLER                PIC  X(004) VALUE 'PA01'.
291295         10  FILLER                PIC  X(004) VALUE 'Y060'.
291295     05  FILLER.
291295         10  FILLER                PIC  X(004) VALUE 'PA02'.
291295         10  FILLER                PIC  X(004) VALUE 'Y060'.
291295     05  FILLER.
011096         10  FILLER                PIC  X(004) VALUE 'PW12'.
011096         10  FILLER                PIC  X(004) VALUE 'Y060'.
011096     05  FILLER.
011096         10  FILLER                PIC  X(004) VALUE 'PA11'.
011096         10  FILLER                PIC  X(004) VALUE 'Y060'.
    ***70
011096     05  FILLER.
011096         10  FILLER                PIC  X(004) VALUE 'PA12'.
011096         10  FILLER                PIC  X(004) VALUE 'Y060'.
011096     05  FILLER.
011096         10  FILLER                PIC  X(004) VALUE 'PA14'.
011096         10  FILLER                PIC  X(004) VALUE 'Y060'.
281096     05  FILLER.
281096         10  FILLER                PIC  X(004) VALUE 'PA15'.
011096         10  FILLER                PIC  X(004) VALUE 'Y060'.
011896     05  FILLER.
061196         10  FILLER                PIC  X(004) VALUE 'GG01'.
061196         10  FILLER                PIC  X(004) VALUE 'Y060'.
011896     05  FILLER.
061196         10  FILLER                PIC  X(004) VALUE 'XY05'.
061196         10  FILLER                PIC  X(004) VALUE 'Y060'.
    ***75
011896     05  FILLER.
061196         10  FILLER                PIC  X(004) VALUE 'XY10'.
061196         10  FILLER                PIC  X(004) VALUE 'Y060'.
011896     05  FILLER.
061196         10  FILLER                PIC  X(004) VALUE 'XY15'.
061196         10  FILLER                PIC  X(004) VALUE 'Y060'.
011896     05  FILLER.
061196         10  FILLER                PIC  X(004) VALUE 'XY20'.
061196         10  FILLER                PIC  X(004) VALUE 'Y060'.
011896     05  FILLER.
061196         10  FILLER                PIC  X(004) VALUE 'XY25'.
061196         10  FILLER                PIC  X(004) VALUE 'Y060'.
011896     05  FILLER.
061196         10  FILLER                PIC  X(004) VALUE 'XY30'.
061196         10  FILLER                PIC  X(004) VALUE 'Y060'.
    ***80
011896     05  FILLER.
061196         10  FILLER                PIC  X(004) VALUE 'XY35'.
061196         10  FILLER                PIC  X(004) VALUE 'Y060'.
011896     05  FILLER.
061196         10  FILLER                PIC  X(004) VALUE 'XY40'.
061196         10  FILLER                PIC  X(004) VALUE 'Y060'.
011896     05  FILLER.
061196         10  FILLER                PIC  X(004) VALUE 'PK11'.
061196         10  FILLER                PIC  X(004) VALUE 'Y060'.
191296     05  FILLER.
191296         10  FILLER                PIC  X(004) VALUE 'SF30'.
191296         10  FILLER                PIC  X(004) VALUE 'Y025'.
191296     05  FILLER.
191296         10  FILLER                PIC  X(004) VALUE 'SF31'.
191296         10  FILLER                PIC  X(004) VALUE 'Y060'.
    ***85
           05  FILLER.
               10  FILLER                PIC  X(004) VALUE 'SF32'.
               10  FILLER                PIC  X(004) VALUE 'Y060'.
           05  FILLER.
               10  FILLER                PIC  X(004) VALUE 'XA11'.
               10  FILLER                PIC  X(004) VALUE 'Y060'.
           05  FILLER.
               10  FILLER                PIC  X(004) VALUE 'XA12'.
               10  FILLER                PIC  X(004) VALUE 'Y060'.
           05  FILLER.
               10  FILLER                PIC  X(004) VALUE 'XA13'.
               10  FILLER                PIC  X(004) VALUE 'Y060'.
           05  FILLER.
               10  FILLER                PIC  X(004) VALUE 'PV11'.
               10  FILLER                PIC  X(004) VALUE 'Y060'.
    ***90
           05  FILLER.
               10  FILLER                PIC  X(004) VALUE 'ZA11'.
               10  FILLER                PIC  X(004) VALUE 'Y060'.
           05  FILLER.
               10  FILLER                PIC  X(004) VALUE 'PV12'.
               10  FILLER                PIC  X(004) VALUE 'Y060'.
           05  FILLER.
               10  FILLER                PIC  X(004) VALUE 'ZA12'.
               10  FILLER                PIC  X(004) VALUE 'Y060'.
           05  FILLER.
               10  FILLER                PIC  X(004) VALUE 'ZV11'.
               10  FILLER                PIC  X(004) VALUE 'Y060'.
           05  FILLER.
               10  FILLER                PIC  X(004) VALUE 'ZV12'.
               10  FILLER                PIC  X(004) VALUE 'Y060'.
    ***95
           05  FILLER.
               10  FILLER                PIC  X(004) VALUE 'PG11'.
               10  FILLER                PIC  X(004) VALUE 'Y060'.
           05  FILLER.
               10  FILLER                PIC  X(004) VALUE 'PG12'.
               10  FILLER                PIC  X(004) VALUE 'Y060'.
           05  FILLER.
               10  FILLER                PIC  X(004) VALUE 'PG13'.
               10  FILLER                PIC  X(004) VALUE 'Y060'.
           05  FILLER.
               10  FILLER                PIC  X(004) VALUE 'PG14'.
               10  FILLER                PIC  X(004) VALUE 'Y060'.
           05  FILLER.
               10  FILLER                PIC  X(004) VALUE 'PG15'.
               10  FILLER                PIC  X(004) VALUE 'Y060'.
    ***100
           05  FILLER.
               10  FILLER                PIC  X(004) VALUE 'PG16'.
               10  FILLER                PIC  X(004) VALUE 'Y060'.
           05  FILLER.
               10  FILLER                PIC  X(004) VALUE 'PG17'.
               10  FILLER                PIC  X(004) VALUE 'Y060'.
           05  FILLER.
               10  FILLER                PIC  X(004) VALUE 'PG18'.
               10  FILLER                PIC  X(004) VALUE 'Y060'.
           05  FILLER.
               10  FILLER                PIC  X(004) VALUE 'SF41'.
               10  FILLER                PIC  X(004) VALUE 'Y060'.
           05  FILLER.
               10  FILLER                PIC  X(004) VALUE 'SF42'.
               10  FILLER                PIC  X(004) VALUE 'Y060'.
    ***105
           05  FILLER.
               10  FILLER                PIC  X(004) VALUE 'PG20'.
               10  FILLER                PIC  X(004) VALUE 'Y060'.

       01  T-TRANSSCHALT-TAB-RED REDEFINES T-TRANSSCHALT-TAB.
180494*     05  T-TRSCHALT-ZEILE OCCURS  33
140994*     05  T-TRSCHALT-ZEILE OCCURS  34
041194*     05  T-TRSCHALT-ZEILE OCCURS  36
070395*     05  T-TRSCHALT-ZEILE OCCURS  44
240795*     05  T-TRSCHALT-ZEILE OCCURS  50
240795*     05  T-TRSCHALT-ZEILE OCCURS  51
120995*     05  T-TRSCHALT-ZEILE OCCURS  52
020296*     05  T-TRSCHALT-ZEILE OCCURS  80
150296*     05  T-TRSCHALT-ZEILE OCCURS  82
260396*     05  T-TRSCHALT-ZEILE OCCURS  70
210696*     05  T-TRSCHALT-ZEILE OCCURS  68
011096*     05  T-TRSCHALT-ZEILE OCCURS  72
061196*     05  T-TRSCHALT-ZEILE OCCURS  73
281096*     05  T-TRSCHALT-ZEILE OCCURS  74
071196*     05  T-TRSCHALT-ZEILE OCCURS  82
191296*     05  T-TRSCHALT-ZEILE OCCURS  83
191296*     05  T-TRSCHALT-ZEILE OCCURS  88
030397*     05  T-TRSCHALT-ZEILE OCCURS  89
300597*     05  T-TRSCHALT-ZEILE OCCURS  93
281097*     05  T-TRSCHALT-ZEILE OCCURS  105
101197      05  T-TRSCHALT-ZEILE OCCURS  106
                      INDEXED BY I-TRSCHALT.
               10  T-TRANS-NORMAL        PIC  X(004).
               10  T-TRANS-SCHALT        PIC  X(004).
      *
      *    ACHTUNG! BEI ERGAENZUNGEN ANPASSUNGEN NOTWENDIG:
      *             - OCCURS-WERT
      *
      ******** ENDE COPY-MEMBER TRANS VORSCHALTPROGRAMM (DRAT0019) ****
