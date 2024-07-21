      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.05.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1751.CPY  $
      *    $Revision:   2.19  $
      *
      *    $Log:   L:/MF/CPY/DRPA1751.CPv  $
      *
      *   Rev 2.19   Jan 19 2021 12:39:12   u130076
      *DAISY-3189 PGVs 3002 und 3004 
      *
      *   Rev 2.18   Jan 06 2021 10:05:10   u130076
      *DAISY-3169 - Anpassung 3001 und 3003
      *
      *   Rev 2.17   Sep 19 2016 13:12:48   T13EX44
      *APA02151: BasisRente verteilte Abschlussprovisionen
      *
      *   Rev 2.16   Jun 01 2016 12:05:30   t13ex24
      *APA02223: Bei PGV 1005/1008 Erstbewegung Struktur Antrag
      *
      *   Rev 2.15   Feb 03 2016 14:55:36   t13ex24
      *FPA01486: Vorbewegung für AML PGV's 1086 und 1087 nicht zwin
      *gend
      *
      *   Rev 2.14   Jul 14 2015 16:59:34   t13ex44
      *FPA01455: Korrektur PGV-Liste
      *
      *   Rev 2.13   Jun 22 2015 13:54:18   t13ex44
      *APA02038: 20-VEP: laufende Provision
      *
      *   Rev 2.12   May 29 2015 06:57:42   t13ex44
      *APA02038: 20-VEP: laufende Provision
      *
      *   Rev 2.11   May 28 2015 13:08:06   t13ae37
      *APA02020: Ges 70: neuer PGV für Riesteranbieterwechsel
      *
      *   Rev 2.10   May 13 2015 11:16:50   t13ae37
      *APA02020: Ges 70: neuer PGV für Riesteranbieterwechsel
      *
      *   Rev 2.9   Feb 26 2015 09:47:40   t13ex44
      *FPA01428: Korrektur Vorbewegungserrmittlung PGV 1087
      *
      *   Rev 2.8   Feb 10 2015 09:22:12   t13ex44
      *FPA01422: Korrektur PGV 1940
      *
      *   Rev 2.7   Feb 06 2015 10:33:34   t13ex44
      *APA01963: Ges 20- Änderung der Basistabelle und Vorbewegungs
      *ermittlung
      *
      *   Rev 2.6   Feb 04 2015 11:53:04   t13ex44
      *APA01963: Ges 20- Änderung der Basistabelle und Vorbewegungs
      *ermittlung
      *
      *   Rev 2.5   Jan 29 2015 13:30:16   t13ex44
      *APA01963: Ges 20- Änderung der Basistabelle und Vorbewegungs
      *ermittlung
      *
      *   Rev 2.4   Jan 14 2015 10:57:42   t13ex44
      *APA01963: Ges 20- Änderung der Basistabelle und Vorbewegungs
      *ermittlung
      *
      *   Rev 2.3   Sep 01 2014 10:40:50   t13ae39
      *APA01827: Haftungszeitreduzierung Central / neuer PGV
      *
      *   Rev 2.2   Feb 18 2014 11:13:46   t13ex72
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g): GeVo Leben (CBO)
      *
      *   Rev 2.1   Jan 10 2014 13:26:48   t13ex72
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g): GeVo Leben (CBO)
      *
      *   Rev 2.0   May 28 2013 14:08:58   t13ex72
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1751                                      *
      *    --------------------------------------------------------   *
      *    KONSTANTENTABELLE                                          *
      *    --------------------------------------------------------   *
      *    ERSTELLT: BÖTTCHER                                         *
      *    DATUM:    28.05.2013                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA01526!28.05.13! 01 ! NEU                       ! BÖTTCHER *
      * APA01526!28.01.14! 2.2! OCCURS VON 200 AUF 147    ! BÖTTCHER *
      * APA01827!11.08.14! 2.3! Neu: CKV GEVO-NEUTRAL 4065! Strauss  *
      *         !        !    ! OCCURS VON 147 AUF 148    !          *
      * APA01963!05.01.15! 2.4! ANPASSUNGEN 1900, 1940,   ! SCHEIDEL *
      *         !        !    ! ERWEITERUNG VBSUCHE-PGV   !          *
      * APA02038!30.04.15!2.91! ANPASSUNGEN 2945, 2984,   ! BUCH     *
      *         !        !    ! 4945, 4984 WEGEN PGV 1941 !          *
      * FPA01445!14.07.15!2.14! INDICES ERWEITERT         ! SCHEIDEL *
      * FPA01486!03.02.16!2.15! Für PGV 1086/1087 Schalter! KÖPRÜCÜ  *
      *         !        !    ! DRPA1751-KZ auf "N"       !          *
      * APA02223!04.05.16!2.16! Für PGV 1008 Schalter     ! KÖPRÜCÜ  *
      *         !        !    ! DRPA1751-KZ auf "N"       !          *
      * APA02151!19.08.16!2.17! VERTEILTE ABSCHLUSSPROV   ! SCHEIDEL *
      * DSY-3170!29.12.20!2.18! PGV 3001 + 3003 SCHALTER  ! KÖPRÜCÜ  *
      *         !        !    ! DRPA1751-KZ auf 'N'       !          *
      * DSY-3190!06.01.21!2.19! PGV 3002 + 3004 SCHALTER  ! KÖPRÜCÜ  *
      *         !        !    ! DRPA1751-KZ auf 'N'       !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESE TABELLE ENTHÄLT DIE SUCHPARAMETER-VORBEWEGUNG       *
      *    FÜR DEN GEVO LEBEN                                        *
      *                                                              *
      ****************************************************************
      *
       01  DRPA1751.
           05  FILLER                    PIC X(026)
                                         VALUE '*** SUCHTABELLE ***'.
      *
           05  DRPA1751-VBSUCHE-TAB-MAX  PIC S9(004) BINARY VALUE 165.
 
      *    KONFIGURATIONSTABELLE FÜR PGV-VORBEWEGUNGEN
           05  DRPA1751-VBSUCHE-TAB.
               15 DRPA1751-VBSUCHE-ZEILE OCCURS 165
                                         INDEXED BY DRPA1751-IX.
      *
      *               PGV
                  20  DRPA1751-PGV       PIC S9(04) BINARY.
      *
      *               ZULÄSSIGE VORBEWEGUNG ERFORDERLICH
      *               J/N
                  20  DRPA1751-KZ        PIC  X(01).
      *
      *               ZULÄSSIGE VORBEWEGUNGEN FÜR DIESEN PGV
                  20  DRPA1751-VBSUCHE-PGV
050115*                                  PIC  X(50).
050115                                   PIC  X(200).
 
      *               FUNKTIONSNR. FÜR PE-STRUKTURSUCHE IN UPPA0082
                  20  DRPA1751-FUNKTION  PIC S9(04) BINARY.
 
           05 DRPA1751-TAB-INHALT.
 
      *        >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      *        FUNKTION 40
      *        >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      *
      * 1940   --------------------------------------------------------
      *        RIESTER ERHÖHUNG VERTEILT AUF 10 JAHRE
      *        --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 1940.
100215         10 FILLER                 PIC X(01)        VALUE 'N'.
100215*        10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)       VALUE SPACES.
               10 FILLER                 PIC S9(4) BINARY VALUE +040.
 
300415*
300415* 1941   --------------------------------------------------------
300415*        VEP LAUFENDE PROVISION
300415*        --------------------------------------------------------
300415         10 FILLER                 PIC S9(4) BINARY VALUE 1941.
300415         10 FILLER                 PIC X(01)        VALUE 'J'.
300415         10 FILLER                 PIC X(200)       VALUE SPACES.
300415         10 FILLER                 PIC S9(4) BINARY VALUE 1941.
190816*
190816* 1942   --------------------------------------------------------
190816*        VEP LAUFENDE PROVISION
190816*        --------------------------------------------------------
190816         10 FILLER                 PIC S9(4) BINARY VALUE 1942.
190816         10 FILLER                 PIC X(01)        VALUE 'J'.
190816         10 FILLER                 PIC X(200)       VALUE SPACES.
190816         10 FILLER                 PIC S9(4) BINARY VALUE 1942.
190816*
190816* 1943   --------------------------------------------------------
190816*        VEP LAUFENDE PROVISION
190816*        --------------------------------------------------------
190816         10 FILLER                 PIC S9(4) BINARY VALUE 1943.
190816         10 FILLER                 PIC X(01)        VALUE 'J'.
190816         10 FILLER                 PIC X(200)       VALUE SPACES.
190816         10 FILLER                 PIC S9(4) BINARY VALUE 1943.
190816*
190816* 1944   --------------------------------------------------------
190816*        VEP LAUFENDE PROVISION
190816*        --------------------------------------------------------
190816         10 FILLER                 PIC S9(4) BINARY VALUE 1944.
190816         10 FILLER                 PIC X(01)        VALUE 'J'.
190816         10 FILLER                 PIC X(200)       VALUE SPACES.
190816         10 FILLER                 PIC S9(4) BINARY VALUE 1944.
190816*
190816* 1985   --------------------------------------------------------
190816*        VEP LAUFENDE PROVISION
190816*        --------------------------------------------------------
190816         10 FILLER                 PIC S9(4) BINARY VALUE 1985.
190816         10 FILLER                 PIC X(01)        VALUE 'J'.
190816         10 FILLER                 PIC X(200)
190816            VALUE '4942,4944,4985'.
190816         10 FILLER                 PIC S9(4) BINARY VALUE 1985.
190816*
190816* 2942   --------------------------------------------------------
190816*        VEP LAUFENDE PROVISION
190816*        --------------------------------------------------------
190816         10 FILLER                 PIC S9(4) BINARY VALUE 2942.
190816         10 FILLER                 PIC X(01)        VALUE 'J'.
190816         10 FILLER                 PIC X(200)
190816            VALUE '1942,1985,3942,3985'.
190816         10 FILLER                 PIC S9(4) BINARY VALUE +49.
190816*
190816* 2943   --------------------------------------------------------
190816*        VEP LAUFENDE PROVISION
190816*        --------------------------------------------------------
190816         10 FILLER                 PIC S9(4) BINARY VALUE 2943.
190816         10 FILLER                 PIC X(01)        VALUE 'J'.
190816         10 FILLER                 PIC X(200)
190816            VALUE '1943,1984,3943,3984'.
190816         10 FILLER                 PIC S9(4) BINARY VALUE +49.
190816*
190816* 2944   --------------------------------------------------------
190816*        VEP LAUFENDE PROVISION
190816*        --------------------------------------------------------
190816         10 FILLER                 PIC S9(4) BINARY VALUE 2944.
190816         10 FILLER                 PIC X(01)        VALUE 'J'.
190816         10 FILLER                 PIC X(200)
190816            VALUE '1944,1985,3944,3985'.
190816         10 FILLER                 PIC S9(4) BINARY VALUE +49.
190816*
190816* 2985   --------------------------------------------------------
190816*        VEP LAUFENDE PROVISION
190816*        --------------------------------------------------------
190816         10 FILLER                 PIC S9(4) BINARY VALUE 2985.
190816         10 FILLER                 PIC X(01)        VALUE 'J'.
190816         10 FILLER                 PIC X(200)
190816            VALUE '1942,1944,1985,3942,3944,3985'.
190816         10 FILLER                 PIC S9(4) BINARY VALUE +53.
190816*
190816* 3942   --------------------------------------------------------
190816*        VEP LAUFENDE PROVISION
190816*        --------------------------------------------------------
190816         10 FILLER                 PIC S9(4) BINARY VALUE 3942.
190816         10 FILLER                 PIC X(01)        VALUE 'J'.
190816         10 FILLER                 PIC X(200)
190816            VALUE '2942,2985'.
190816         10 FILLER                 PIC S9(4) BINARY VALUE +49.
190816*
190816* 3943   --------------------------------------------------------
190816*        VEP LAUFENDE PROVISION
190816*        --------------------------------------------------------
190816         10 FILLER                 PIC S9(4) BINARY VALUE 3943.
190816         10 FILLER                 PIC X(01)        VALUE 'J'.
190816         10 FILLER                 PIC X(200)
190816            VALUE '2943,2984'.
190816         10 FILLER                 PIC S9(4) BINARY VALUE +49.
190816*
190816* 3944   --------------------------------------------------------
190816*        VEP LAUFENDE PROVISION
190816*        --------------------------------------------------------
190816         10 FILLER                 PIC S9(4) BINARY VALUE 3944.
190816         10 FILLER                 PIC X(01)        VALUE 'J'.
190816         10 FILLER                 PIC X(200)
190816            VALUE '2944,2985'.
190816         10 FILLER                 PIC S9(4) BINARY VALUE +49.
190816*
190816* 3985   --------------------------------------------------------
190816*        VEP LAUFENDE PROVISION
190816*        --------------------------------------------------------
190816         10 FILLER                 PIC S9(4) BINARY VALUE 3985.
190816         10 FILLER                 PIC X(01)        VALUE 'J'.
190816         10 FILLER                 PIC X(200)
190816            VALUE '2942,2944,2985'.
190816         10 FILLER                 PIC S9(4) BINARY VALUE +49.
190816*
190816* 4942   --------------------------------------------------------
190816*        VEP LAUFENDE PROVISION
190816*        --------------------------------------------------------
190816         10 FILLER                 PIC S9(4) BINARY VALUE 4942.
190816         10 FILLER                 PIC X(01)        VALUE 'J'.
190816         10 FILLER                 PIC X(200)
190816            VALUE '1942,1985,3942,3985'.
190816         10 FILLER                 PIC S9(4) BINARY VALUE +49.
190816*
190816* 4943   --------------------------------------------------------
190816*        VEP LAUFENDE PROVISION
190816*        --------------------------------------------------------
190816         10 FILLER                 PIC S9(4) BINARY VALUE 4943.
190816         10 FILLER                 PIC X(01)        VALUE 'J'.
190816         10 FILLER                 PIC X(200)
190816            VALUE '1943,1984,3943,3984'.
190816         10 FILLER                 PIC S9(4) BINARY VALUE +49.
190816*
190816* 4944   --------------------------------------------------------
190816*        VEP LAUFENDE PROVISION
190816*        --------------------------------------------------------
190816         10 FILLER                 PIC S9(4) BINARY VALUE 4944.
190816         10 FILLER                 PIC X(01)        VALUE 'J'.
190816         10 FILLER                 PIC X(200)
190816            VALUE '1944,1985,3944,3985'.
190816         10 FILLER                 PIC S9(4) BINARY VALUE +49.
190816*
190816* 4985   --------------------------------------------------------
190816*        VEP LAUFENDE PROVISION
190816*        --------------------------------------------------------
190816         10 FILLER                 PIC S9(4) BINARY VALUE 4985.
190816         10 FILLER                 PIC X(01)        VALUE 'J'.
190816         10 FILLER                 PIC X(200)
190816            VALUE '1942,1944,1985,3942,3944,3985'.
190816         10 FILLER                 PIC S9(4) BINARY VALUE +49.
 
      *        >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      *        FUNKTION 41
      *        >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      *
      * 1001   --------------------------------------------------------
      *        DIE PGV AUS DENEN DIE STRUKTUR *NICHT* ERMITTELT WERDEN
      *        DARF SIND DIREKT IM SQL(ZJPA0017) FUNKTION 41 ABGELEGT
      *        --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 1001.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)       VALUE SPACES.
               10 FILLER                 PIC S9(4) BINARY VALUE +041.
      * 1002   --------------------------------------------------------
      *        DIE PGV AUS DENEN DIE STRUKTUR *NICHT* ERMITTELT WERDEN
      *        DARF SIND DIREKT IM SQL(ZJPA0017) FUNKTION 41 ABGELEGT
      *        --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 1002.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)       VALUE SPACES.
               10 FILLER                 PIC S9(4) BINARY VALUE +041.
      * 2001   --------------------------------------------------------
      *        DIE PGV AUS DENEN DIE STRUKTUR *NICHT* ERMITTELT WERDEN
      *        DARF SIND DIREKT IM SQL(ZJPA0017) FUNKTION 41 ABGELEGT
      *        --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 2001.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)       VALUE SPACES.
               10 FILLER                 PIC S9(4) BINARY VALUE +041.
      * 2002   --------------------------------------------------------
      *        DIE PGV AUS DENEN DIE STRUKTUR *NICHT* ERMITTELT WERDEN
      *        DARF SIND DIREKT IM SQL(ZJPA0017) FUNKTION 41 ABGELEGT
      *        --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 2002.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)       VALUE SPACES.
               10 FILLER                 PIC S9(4) BINARY VALUE +041.
      * 2005   --------------------------------------------------------
      *        DIE PGV AUS DENEN DIE STRUKTUR *NICHT* ERMITTELT WERDEN
      *        DARF SIND DIREKT IM SQL(ZJPA0017) FUNKTION 41 ABGELEGT
      *        --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 2005.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)       VALUE SPACES.
               10 FILLER                 PIC S9(4) BINARY VALUE +041.
      * 3001   --------------------------------------------------------
      *        DIE PGV AUS DENEN DIE STRUKTUR *NICHT* ERMITTELT WERDEN
      *        DARF SIND DIREKT IM SQL(ZJPA0017) FUNKTION 41 ABGELEGT
      *        --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 3001.
291220*        10 FILLER                 PIC X(01)        VALUE 'J'.
291220         10 FILLER                 PIC X(01)        VALUE 'N'.
               10 FILLER                 PIC X(200)       VALUE SPACES.
               10 FILLER                 PIC S9(4) BINARY VALUE +041.
      * 3002   --------------------------------------------------------
      *        DIE PGV AUS DENEN DIE STRUKTUR *NICHT* ERMITTELT WERDEN
      *        DARF SIND DIREKT IM SQL(ZJPA0017) FUNKTION 41 ABGELEGT
      *        --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 3002.
060121*        10 FILLER                 PIC X(01)        VALUE 'J'.
060121         10 FILLER                 PIC X(01)        VALUE 'N'.
               10 FILLER                 PIC X(200)       VALUE SPACES.
               10 FILLER                 PIC S9(4) BINARY VALUE +041.
      * 3005   --------------------------------------------------------
      *        DIE PGV AUS DENEN DIE STRUKTUR *NICHT* ERMITTELT WERDEN
      *        DARF SIND DIREKT IM SQL(ZJPA0017) FUNKTION 41 ABGELEGT
      *        --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 3005.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)       VALUE SPACES.
               10 FILLER                 PIC S9(4) BINARY VALUE +041.
      * 4001   --------------------------------------------------------
      *        DIE PGV AUS DENEN DIE STRUKTUR *NICHT* ERMITTELT WERDEN
      *        DARF SIND DIREKT IM SQL(ZJPA0017) FUNKTION 41 ABGELEGT
      *        --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 4001.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)       VALUE SPACES.
               10 FILLER                 PIC S9(4) BINARY VALUE +041.
      * 4002   --------------------------------------------------------
      *        DIE PGV AUS DENEN DIE STRUKTUR *NICHT* ERMITTELT WERDEN
      *        DARF SIND DIREKT IM SQL(ZJPA0017) FUNKTION 41 ABGELEGT
      *        --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 4002.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)       VALUE SPACES.
               10 FILLER                 PIC S9(4) BINARY VALUE +041.
      * 4005   --------------------------------------------------------
      *        DIE PGV AUS DENEN DIE STRUKTUR *NICHT* ERMITTELT WERDEN
      *        DARF SIND DIREKT IM SQL(ZJPA0017) FUNKTION 41 ABGELEGT
      *        --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 4005.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)       VALUE SPACES.
               10 FILLER                 PIC S9(4) BINARY VALUE +041.
 
      *        >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      *        FUNKTION 42
      *        >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      *
      * 1008   --------------------------------------------------------
      *        DIE PGV AUS DENEN DIE STRUKTUR ERMITTELT WERDEN DÜRFEN
      *        SIND DIREKT IM SQL(ZJPA0017) FUNKTION 42 ABGELEGT
      *        --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 1008.
040516*        10 FILLER                 PIC X(01)        VALUE 'J'.
040516         10 FILLER                 PIC X(01)        VALUE 'N'.
               10 FILLER                 PIC X(200)       VALUE SPACES.
               10 FILLER                 PIC S9(4) BINARY VALUE +042.
 
      *        >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      *        FUNKTION 44
      *        >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      *
      * 1005   --------------------------------------------------------
      *        DIE PGV AUS DENEN DIE STRUKTUR *NICHT* ERMITTELT WERDEN
      *        DARF SIND DIREKT IM SQL(ZJPA0017) FUNKTION 44 ABGELEGT
      *        --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 1005.
      *        -- KEINE VORBEWEGUNG ERFORDERLICH --
               10 FILLER                 PIC X(01)        VALUE 'N'.
               10 FILLER                 PIC X(200)       VALUE SPACES.
               10 FILLER                 PIC S9(4) BINARY VALUE +044.
 
      *        >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      *        FUNKTION 43 STRUKTUR AV
      *        >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      *
      * 1953   --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 1953.
               10 FILLER                 PIC X(01)        VALUE 'N'.
               10 FILLER                 PIC X(200)
                  VALUE '4953'.
               10 FILLER                 PIC S9(4) BINARY VALUE +043.
      * 1960   --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 1960.
               10 FILLER                 PIC X(01)        VALUE 'N'.
               10 FILLER                 PIC X(200)
                  VALUE '4905,4980'.
               10 FILLER                 PIC S9(4) BINARY VALUE +043.
      * 1962   --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 1962.
               10 FILLER                 PIC X(01)        VALUE 'N'.
               10 FILLER                 PIC X(200)
                  VALUE '4925'.
               10 FILLER                 PIC S9(4) BINARY VALUE +043.
      * 1963   --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 1963.
               10 FILLER                 PIC X(01)        VALUE 'N'.
               10 FILLER                 PIC X(200)
                  VALUE '4935'.
               10 FILLER                 PIC S9(4) BINARY VALUE +043.
      * 1964   --------------------------------------------------------
      *        WIK NACH STORNO (RIESTER ERHÖHUNG VERTEILT AUF 10J.)
      *        --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 1964.
               10 FILLER                 PIC X(01)        VALUE 'N'.
               10 FILLER                 PIC X(200)
                  VALUE '4945'.
               10 FILLER                 PIC S9(4) BINARY VALUE +043.
      * 1968   --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 1968.
               10 FILLER                 PIC X(01)        VALUE 'N'.
               10 FILLER                 PIC X(200)
                  VALUE '4908'.
               10 FILLER                 PIC S9(4) BINARY VALUE +043.
      * 1970   --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 1970.
               10 FILLER                 PIC X(01)        VALUE 'N'.
               10 FILLER                 PIC X(200)
                  VALUE '4900,4990'.
               10 FILLER                 PIC S9(4) BINARY VALUE +043.
      * 1972   --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 1972.
               10 FILLER                 PIC X(01)        VALUE 'N'.
               10 FILLER                 PIC X(200)
                  VALUE '4920'.
               10 FILLER                 PIC S9(4) BINARY VALUE +043.
      * 1978   --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 1978.
               10 FILLER                 PIC X(01)        VALUE 'N'.
               10 FILLER                 PIC X(200)
                  VALUE '4903'.
               10 FILLER                 PIC S9(4) BINARY VALUE +043.
      * 1979   --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 1979.
               10 FILLER                 PIC X(01)        VALUE 'N'.
               10 FILLER                 PIC X(200)
                  VALUE '4919'.
               10 FILLER                 PIC S9(4) BINARY VALUE +043.
      * 3900   --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 3900.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)
                  VALUE '2900,2902,2990'.
               10 FILLER                 PIC S9(4) BINARY VALUE +043.
      * 3902   --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 3902.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)
                  VALUE '2900,2902'.
               10 FILLER                 PIC S9(4) BINARY VALUE +043.
      * 3903   --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 3903.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)
                  VALUE '2903'.
               10 FILLER                 PIC S9(4) BINARY VALUE +043.
      * 3905   --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 3905.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)
                  VALUE '2905,2907,2980'.
               10 FILLER                 PIC S9(4) BINARY VALUE +043.
      * 3907   --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 3907.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)
                  VALUE '2905,2907,2925,2935'.
               10 FILLER                 PIC S9(4) BINARY VALUE +043.
      * 3908   --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 3908.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)
                  VALUE '2908'.
               10 FILLER                 PIC S9(4) BINARY VALUE +043.
      * 3912   --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 3912.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)
                  VALUE '2912,2919'.
               10 FILLER                 PIC S9(4) BINARY VALUE +043.
      * 3913   --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 3913.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)
                  VALUE '2913'.
               10 FILLER                 PIC S9(4) BINARY VALUE +043.
      * 3919   --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 3919.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)
                  VALUE '2919'.
               10 FILLER                 PIC S9(4) BINARY VALUE +043.
      * 3920   --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 3920.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)
                  VALUE '2902,2920'.
               10 FILLER                 PIC S9(4) BINARY VALUE +043.
      * 3925   --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 3925.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)
                  VALUE '2902,2925'.
               10 FILLER                 PIC S9(4) BINARY VALUE +043.
      * 3935   --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 3935.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)
                  VALUE '2902,2935'.
               10 FILLER                 PIC S9(4) BINARY VALUE +043.
      * 3945   --------------------------------------------------------
      *        WIK (RIESTER ERHÖHUNG VERTEILT AUF 10J.)
      *        --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 3945.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)
                  VALUE '2945'.
               10 FILLER                 PIC S9(4) BINARY VALUE +043.
 
      *        >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      *        FUNKTION 45
      *        >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      *
      * 1903   --------------------------------------------------------
      *        MEHRSTUFIGE VORBEWEGUNGSERMITTLUNG
      *        --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 1903.
               10 FILLER                 PIC X(01)        VALUE 'N'.
               10 FILLER                 PIC X(200)       VALUE SPACES.
               10 FILLER                 PIC S9(4) BINARY VALUE +045.
 
      *        >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      *        FUNKTION 46
      *        >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      *
      * 1900   --------------------------------------------------------
      *        NEUZUGANG - HIER DARF ES IN EINIGEN FÄLLEN AUCH EINEN
      *        DOPPELTEN NEUZUGANG GEBEN.
      *        --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 1900.
               10 FILLER                 PIC X(01)        VALUE 'N'.
               10 FILLER                 PIC X(200)       VALUE SPACES.
               10 FILLER                 PIC S9(4) BINARY VALUE +046.
 
      *        >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      *        FUNKTION 47
      *        >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      *
      * 1919   --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 1919.
               10 FILLER                 PIC X(01)        VALUE 'N'.
               10 FILLER                 PIC X(200)       VALUE SPACES.
               10 FILLER                 PIC S9(4) BINARY VALUE +047.
 
      *        >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      *        FUNKTION 48 STRUKTUR AV
      *        >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      *
      * 1958   --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 1958.
050115*        10 FILLER                 PIC X(01)        VALUE 'N'.
050115         10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)
                  VALUE '4958'.
050115*        10 FILLER                 PIC S9(4) BINARY VALUE +048.
050115         10 FILLER                 PIC S9(4) BINARY VALUE +053.
      * 1980   --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 1980.
               10 FILLER                 PIC X(01)        VALUE 'N'.
               10 FILLER                 PIC X(200)
                  VALUE '4905,4907,4980'.
               10 FILLER                 PIC S9(4) BINARY VALUE +048.
      * 1982   --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 1982.
               10 FILLER                 PIC X(01)        VALUE 'N'.
               10 FILLER                 PIC X(200)
                  VALUE '4925,4982'.
               10 FILLER                 PIC S9(4) BINARY VALUE +048.
      * 1983   --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 1983.
               10 FILLER                 PIC X(01)        VALUE 'N'.
               10 FILLER                 PIC X(200)
                  VALUE '4935,4983'.
               10 FILLER                 PIC S9(4) BINARY VALUE +048.
      * 1984   --------------------------------------------------------
      *        ÄNDERUNGSZUGANG (RIESTER ERHÖHUNG VERTEILT AUF 10J.)
      *        --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 1984.
               10 FILLER                 PIC X(01)        VALUE 'N'.
               10 FILLER                 PIC X(200)
                  VALUE '4943,4945,4984'.
               10 FILLER                 PIC S9(4) BINARY VALUE +048.
      * 1988   --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 1988.
050115*        10 FILLER                 PIC X(01)        VALUE 'N'.
050115         10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)
                  VALUE '4908,4988'.
050115*        10 FILLER                 PIC S9(4) BINARY VALUE +048.
050115         10 FILLER                 PIC S9(4) BINARY VALUE +053.
      * 1989   --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 1989.
               10 FILLER                 PIC X(01)        VALUE 'N'.
               10 FILLER                 PIC X(200)
                  VALUE '4919,4989'.
               10 FILLER                 PIC S9(4) BINARY VALUE +048.
      * 1990   --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 1990.
               10 FILLER                 PIC X(01)        VALUE 'N'.
               10 FILLER                 PIC X(200)
                  VALUE '4900,4902,4990'.
               10 FILLER                 PIC S9(4) BINARY VALUE +048.
      * 1992   --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 1992.
               10 FILLER                 PIC X(01)        VALUE 'N'.
               10 FILLER                 PIC X(200)
                  VALUE '4920,4992'.
               10 FILLER                 PIC S9(4) BINARY VALUE +048.
      * 1998   --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 1998.
050115*        10 FILLER                 PIC X(01)        VALUE 'N'.
050115         10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)
                  VALUE '4903,4998'.
050115*        10 FILLER                 PIC S9(4) BINARY VALUE +048.
050115         10 FILLER                 PIC S9(4) BINARY VALUE +053.
 
      *        >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      *        FUNKTION 49 STRUKTUR AV
      *        >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      *
      * 1022   --------------------------------------------------------
      *        GEVO NEUTRAL
      *        --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 1022.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)
                  VALUE '1086,1087,2086,2087,3086,3087,4086,4087'.
               10 FILLER                 PIC S9(4) BINARY VALUE +049.
      * 1033   --------------------------------------------------------
      *        GEVO NEUTRAL
      *        --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 1033.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)
                  VALUE '1086,1087,2086,2087,3086,3087,4086,4087'.
               10 FILLER                 PIC S9(4) BINARY VALUE +049.
      * 1044   --------------------------------------------------------
      *        GEVO NEUTRAL
      *        --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 1044.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)
                  VALUE '1086,1087,2086,2087,3086,3087,4086,4087'.
               10 FILLER                 PIC S9(4) BINARY VALUE +049.
      * 1055   --------------------------------------------------------
      *        GEVO NEUTRAL
      *        --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 1055.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)
                  VALUE '1086,1087,2086,2087,3086,3087,4086,4087'.
               10 FILLER                 PIC S9(4) BINARY VALUE +049.
      * 2022   --------------------------------------------------------
      *        GEVO NEUTRAL
      *        --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 2022.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)
                  VALUE '1086,1087,2086,2087,3086,3087,4086,4087'.
               10 FILLER                 PIC S9(4) BINARY VALUE +049.
      * 2033   --------------------------------------------------------
      *        GEVO NEUTRAL
      *        --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 2033.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)
                  VALUE '1086,1087,2086,2087,3086,3087,4086,4087'.
               10 FILLER                 PIC S9(4) BINARY VALUE +049.
      * 2044   --------------------------------------------------------
      *        GEVO NEUTRAL
      *        --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 2044.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)
                  VALUE '1086,1087,2086,2087,3086,3087,4086,4087'.
               10 FILLER                 PIC S9(4) BINARY VALUE +049.
      * 2055   --------------------------------------------------------
      *        GEVO NEUTRAL
      *        --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 2055.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)
                  VALUE '1086,1087,2086,2087,3086,3087,4086,4087'.
               10 FILLER                 PIC S9(4) BINARY VALUE +049.
      * 3022   --------------------------------------------------------
      *        GEVO NEUTRAL
      *        --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 3022.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)
                  VALUE '1086,1087,2086,2087,3086,3087,4086,4087'.
               10 FILLER                 PIC S9(4) BINARY VALUE +049.
      * 3033   --------------------------------------------------------
      *        GEVO NEUTRAL
      *        --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 3033.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)
                  VALUE '1086,1087,2086,2087,3086,3087,4086,4087'.
               10 FILLER                 PIC S9(4) BINARY VALUE +049.
      * 3044   --------------------------------------------------------
      *        GEVO NEUTRAL
      *        --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 3044.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)
                  VALUE '1086,1087,2086,2087,3086,3087,4086,4087'.
               10 FILLER                 PIC S9(4) BINARY VALUE +049.
      * 3055   --------------------------------------------------------
      *        GEVO NEUTRAL
      *        --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 3055.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)
                  VALUE '1086,1087,2086,2087,3086,3087,4086,4087'.
               10 FILLER                 PIC S9(4) BINARY VALUE +049.
      * 4022   --------------------------------------------------------
      *        GEVO NEUTRAL
      *        --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 4022.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)
                  VALUE '1086,1087,2086,2087,3086,3087,4086,4087'.
               10 FILLER                 PIC S9(4) BINARY VALUE +049.
      * 4033   --------------------------------------------------------
      *        GEVO NEUTRAL
      *        --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 4033.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)
                  VALUE '1086,1087,2086,2087,3086,3087,4086,4087'.
               10 FILLER                 PIC S9(4) BINARY VALUE +049.
      * 4044   --------------------------------------------------------
      *        GEVO NEUTRAL
      *        --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 4044.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)
                  VALUE '1086,1087,2086,2087,3086,3087,4086,4087'.
               10 FILLER                 PIC S9(4) BINARY VALUE +049.
      * 4055   --------------------------------------------------------
      *        GEVO NEUTRAL
      *        --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 4055.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)
                  VALUE '1086,1087,2086,2087,3086,3087,4086,4087'.
               10 FILLER                 PIC S9(4) BINARY VALUE +049.
 
110814*        >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
110814*        FUNKTION 01
110814*        >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
110814*
110814* 4065   --------------------------------------------------------
110814*        GEVO NEUTRAL für CKV-CENTRAL
110814*        --------------------------------------------------------
110814         10 FILLER                 PIC S9(4) BINARY VALUE 4065.
110814         10 FILLER                 PIC X(01)        VALUE 'J'.
110814         10 FILLER                 PIC X(200)
110814            VALUE '                                       '.
110814         10 FILLER                 PIC S9(4) BINARY VALUE +001.
110814
      *        >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      *        FUNKTION 49 STRUKTUR AV
      *        >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      *
      * 2900   --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 2900.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)
                  VALUE '1900,1902,1905,1970,1990,3900,3902,3990'.
               10 FILLER                 PIC S9(4) BINARY VALUE +049.
      * 2902   --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 2902.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)
                  VALUE '1902,3902'.
               10 FILLER                 PIC S9(4) BINARY VALUE +049.
      * 2903   --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 2903.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)
                  VALUE '1903,1978,1998,3903,3998'.
               10 FILLER                 PIC S9(4) BINARY VALUE +049.
      * 2905   --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 2905.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)
                  VALUE '1900,1902,1905,1960,1980,3905,3907,3980'.
               10 FILLER                 PIC S9(4) BINARY VALUE +049.
      * 2907   --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 2907.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)
                  VALUE '1902,3907'.
               10 FILLER                 PIC S9(4) BINARY VALUE +049.
      * 2908   --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 2908.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)
                  VALUE '1903,1968,1988,3908,3988'.
               10 FILLER                 PIC S9(4) BINARY VALUE +049.
      * 2912   --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 2912.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)
                  VALUE '1912,3912'.
               10 FILLER                 PIC S9(4) BINARY VALUE +049.
      * 2913   --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 2913.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)
                  VALUE '1913,1953,1958,3913,3958'.
               10 FILLER                 PIC S9(4) BINARY VALUE +049.
      * 2919   --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 2919.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)
                  VALUE '1912,1919,1979,1989,3912,3919,3989'.
               10 FILLER                 PIC S9(4) BINARY VALUE +049.
      * 2920   --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 2920.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)
                  VALUE '1902,1920,1972,1992,3920,3992'.
               10 FILLER                 PIC S9(4) BINARY VALUE +049.
      * 2925   --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 2925.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)
                  VALUE '1902,1920,1962,1982,3925,3982'.
               10 FILLER                 PIC S9(4) BINARY VALUE +049.
      * 2935   --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 2935.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)
                  VALUE '1902,1930,1963,1980,1983,3935,3983'.
               10 FILLER                 PIC S9(4) BINARY VALUE +049.
      * 2945   --------------------------------------------------------
      *        STORNO (RIESTER ERHÖHUNG VERTEILT AUF 10J.)
      *        --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 2945.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)
                  VALUE '1940,1941,1943,1964,1980,1984,3943,3945,3984'.
               10 FILLER                 PIC S9(4) BINARY VALUE +049.
      * 2958   --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 2958.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)
                  VALUE '1913,1953,1958,3913,3958'.
               10 FILLER                 PIC S9(4) BINARY VALUE +049.
      * 2980   --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 2980.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)
                  VALUE '1900,1902,1905,1960,1980,3905,3907,3980'.
               10 FILLER                 PIC S9(4) BINARY VALUE +049.
      * 2982   --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 2982.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)
                  VALUE '1902,1920,1962,1982,3925,3982'.
               10 FILLER                 PIC S9(4) BINARY VALUE +049.
      * 2983   --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 2983.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)
                  VALUE '1930,1963,1983,3935,3983'.
               10 FILLER                 PIC S9(4) BINARY VALUE +049.
      * 2984   --------------------------------------------------------
      *        ÄNDERUNGSABGANG (RIESTER ERHÖHUNG VERTEILT AUF 10J.)
      *        --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 2984.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)
                  VALUE '1940,1941,1943,1964,1984,3943,3945,3984'.
               10 FILLER                 PIC S9(4) BINARY VALUE +049.
      * 2988   --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 2988.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)
                  VALUE '1903,1968,1988,3908,3988'.
               10 FILLER                 PIC S9(4) BINARY VALUE +049.
      * 2989   --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 2989.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)
                  VALUE '1912,1919,1979,1989,3912,3919,3989'.
               10 FILLER                 PIC S9(4) BINARY VALUE +049.
      * 2990   --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 2990.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)
                  VALUE '1900,1902,1905,1970,1990,3900,3902,3990'.
               10 FILLER                 PIC S9(4) BINARY VALUE +049.
      * 2992   --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 2992.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)
                  VALUE '1920,1972,1992,3920,3992'.
               10 FILLER                 PIC S9(4) BINARY VALUE +049.
      * 2998   --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 2998.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)
                  VALUE '1903,1978,1998,3903,3998'.
               10 FILLER                 PIC S9(4) BINARY VALUE +049.
      * 4900   --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 4900.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)
                  VALUE '1900,1902,1970,1990,3900,3902,3990'.
               10 FILLER                 PIC S9(4) BINARY VALUE +049.
      * 4902   --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 4902.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)
                  VALUE '1902,3902'.
               10 FILLER                 PIC S9(4) BINARY VALUE +049.
      * 4903   --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 4903.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)
                  VALUE '1903,1978,1998,3903,3998'.
               10 FILLER                 PIC S9(4) BINARY VALUE +049.
      * 4905   --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 4905.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)
                  VALUE '1900,1902,1905,1960,1980,3905,3907,3980'.
               10 FILLER                 PIC S9(4) BINARY VALUE +049.
      * 4907   --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 4907.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)
                  VALUE '1902,3907'.
               10 FILLER                 PIC S9(4) BINARY VALUE +049.
      * 4908   --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 4908.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)
                  VALUE '1903,1968,1988,3908,3988'.
               10 FILLER                 PIC S9(4) BINARY VALUE +049.
      * 4912   --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 4912.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)
                  VALUE '1912,3912'.
               10 FILLER                 PIC S9(4) BINARY VALUE +049.
      * 4913   --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 4913.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)
                  VALUE '1913,1953,1958,3913,3958'.
               10 FILLER                 PIC S9(4) BINARY VALUE +049.
      * 4919   --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 4919.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)
                  VALUE '1912,1919,1979,1989,3912,3919,3989'.
               10 FILLER                 PIC S9(4) BINARY VALUE +049.
      * 4920   --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 4920.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)
                  VALUE '1902,1920,1972,1992,3920,3992'.
               10 FILLER                 PIC S9(4) BINARY VALUE +049.
      * 4925   --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 4925.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)
                  VALUE '1902,1920,1962,1982,3925,3982'.
               10 FILLER                 PIC S9(4) BINARY VALUE +049.
      * 4935   --------------------------------------------------------
      *        AUFHEBUNG NEUTRAL (RIESTER ERHÖHUNG)
      *        --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 4935.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)
                  VALUE '1902,1930,1963,1983,3935,3983'.
               10 FILLER                 PIC S9(4) BINARY VALUE +049.
      * 4945   --------------------------------------------------------
      *        AUFHEBUNG NEUTRAL (RIESTER ERHÖHUNG VERTEILT AUF 10J.)
      *        --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 4945.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)
                  VALUE '1940,1941,1943,1964,1984,3943,3945,3984'.
               10 FILLER                 PIC S9(4) BINARY VALUE +049.
      * 4958   --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 4958.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)
                  VALUE '1913,1953,1958,3913,3958'.
               10 FILLER                 PIC S9(4) BINARY VALUE +049.
      * 4980   --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 4980.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)
                  VALUE '1900,1902,1905,1960,1980,3905,3907,3980'.
               10 FILLER                 PIC S9(4) BINARY VALUE +049.
      * 4982   --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 4982.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)
                  VALUE '1902,1920,1962,1982,3925,3982'.
               10 FILLER                 PIC S9(4) BINARY VALUE +049.
      * 4983   --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 4983.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)
                  VALUE '1930,1963,1983,3935,3983'.
               10 FILLER                 PIC S9(4) BINARY VALUE +049.
      * 4984   --------------------------------------------------------
      *        ÄNDERUNGSABGANG (RIESTER ERHÖHUNG VERTEILT AUF 10J.)
      *        --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 4984.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)
                  VALUE '1940,1941,1943,1964,1984,3943,3945,3984'.
               10 FILLER                 PIC S9(4) BINARY VALUE +049.
      * 4988   --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 4988.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)
                  VALUE '1903,1968,1988,3908,3988'.
               10 FILLER                 PIC S9(4) BINARY VALUE +049.
      * 4989   --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 4989.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)
                  VALUE '1912,1919,1979,1989,3912,3919,3989'.
               10 FILLER                 PIC S9(4) BINARY VALUE +049.
      * 4990   --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 4990.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)
                  VALUE '1900,1902,1970,1990,3900,3902,3990'.
               10 FILLER                 PIC S9(4) BINARY VALUE +049.
      * 4992   --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 4992.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)
                  VALUE '1920,1972,1992,3920,3992'.
               10 FILLER                 PIC S9(4) BINARY VALUE +049.
      * 4998   --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 4998.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)
                  VALUE '1903,1978,1998,3903,3998'.
               10 FILLER                 PIC S9(4) BINARY VALUE +049.
 
      *        >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      *        FUNKTION 49 STRUKTUR PAUSCHAL
      *        >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      *
      * 1086   --------------------------------------------------------
      *        VORBEWEGUNG MUSS NICHT ZWINGEND VORHANDEN SEIN,
      *        VERWALTET WIRD IMMER MIT PAUSCHALSTRUKTUR
      *        --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 1086.
               10 FILLER                 PIC X(01)        VALUE 'N'.
               10 FILLER                 PIC X(40)
                  VALUE '1005,1900,1902,1903,1920,1930,1940,1941,'.
               10 FILLER                 PIC X(40)
                  VALUE '4086,4900,4902,4903,4920,4945,4990,4992,'.
               10 FILLER                 PIC X(40)
                  VALUE '4998'.
               10 FILLER                 PIC X(080).
               10 FILLER                 PIC S9(4) BINARY VALUE +049.
      * 1087   --------------------------------------------------------
      *        VORBEWEGUNG MUSS NICHT ZWINGEND VORHANDEN SEIN,
      *        VERWALTET WIRD IMMER MIT PAUSCHALSTRUKTUR
      *        --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 1087.
               10 FILLER                 PIC X(01)        VALUE 'N'.
               10 FILLER                 PIC X(40)
                  VALUE '1005,1900,1902,1903,1920,1930,1940,1941,'.
               10 FILLER                 PIC X(40)
                  VALUE '1988,4087,4905,4907,4908,4925,4935,4945,'.
               10 FILLER                 PIC X(40)
                  VALUE '4980,4982,4983'.
               10 FILLER                 PIC X(080).
               10 FILLER                 PIC S9(4) BINARY VALUE +049.
      * 2086   --------------------------------------------------------
      *        PAUSCHALSTRUKTUR
      *        --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 2086.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)
                  VALUE '1086,3086'.
               10 FILLER                 PIC S9(4) BINARY VALUE +049.
      * 2087   --------------------------------------------------------
      *        PAUSCHALSTRUKTUR
      *        --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 2087.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)
                  VALUE '1087,3087'.
               10 FILLER                 PIC S9(4) BINARY VALUE +049.
      * 4086   --------------------------------------------------------
      *        PAUSCHALSTRUKTUR
      *        --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 4086.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)
                  VALUE '1086,3086'.
               10 FILLER                 PIC S9(4) BINARY VALUE +049.
      * 4087   --------------------------------------------------------
      *        PAUSCHALSTRUKTUR
      *        --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 4087.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)
                  VALUE '1087,3087'.
               10 FILLER                 PIC S9(4) BINARY VALUE +049.
 
      *        >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      *        FUNKTION 50 OHNE VORBEWEGUNG,STRUKTUR AV
      *        >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      *
      * 1905   --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 1905.
               10 FILLER                 PIC X(01)        VALUE 'N'.
               10 FILLER                 PIC X(200)
                  VALUE '4905'.
               10 FILLER                 PIC S9(4) BINARY VALUE +050.
      * 1913   --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 1913.
               10 FILLER                 PIC X(01)        VALUE 'N'.
               10 FILLER                 PIC X(200)
                  VALUE '4913,4958'.
               10 FILLER                 PIC S9(4) BINARY VALUE +050.
 
      *        >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      *        FUNKTION 51
      *        >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      *
      * 1003   --------------------------------------------------------
      *        DIE PGV AUS DENEN DIE STRUKTUR ERMITTELT WERDEN DÜRFEN
      *        SIND DIREKT IM SQL(ZJPA0017) FUNKTION 51 ABGELEGT
      *        --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 1003.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)       VALUE SPACES.
               10 FILLER                 PIC S9(4) BINARY VALUE +051.
      * 1004   --------------------------------------------------------
      *        DIE PGV AUS DENEN DIE STRUKTUR ERMITTELT WERDEN DÜRFEN
      *        SIND DIREKT IM SQL(ZJPA0017) FUNKTION 51 ABGELEGT
      *        --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 1004.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)       VALUE SPACES.
               10 FILLER                 PIC S9(4) BINARY VALUE +051.
      * 2003   --------------------------------------------------------
      *        DIE PGV AUS DENEN DIE STRUKTUR ERMITTELT WERDEN DÜRFEN
      *        SIND DIREKT IM SQL(ZJPA0017) FUNKTION 51 ABGELEGT
      *        --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 2003.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)       VALUE SPACES.
               10 FILLER                 PIC S9(4) BINARY VALUE +051.
      * 2004   --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 2004.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)       VALUE SPACES.
               10 FILLER                 PIC S9(4) BINARY VALUE +051.
      * 2008   --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 2008.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)       VALUE SPACES.
               10 FILLER                 PIC S9(4) BINARY VALUE +051.
      * 3003   --------------------------------------------------------
      *        DIE PGV AUS DENEN DIE STRUKTUR ERMITTELT WERDEN DÜRFEN
      *        SIND DIREKT IM SQL(ZJPA0017) FUNKTION 51 ABGELEGT
      *        --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 3003.
291220*        10 FILLER                 PIC X(01)        VALUE 'J'.
291220         10 FILLER                 PIC X(01)        VALUE 'N'.
               10 FILLER                 PIC X(200)       VALUE SPACES.
               10 FILLER                 PIC S9(4) BINARY VALUE +051.
      * 3004   --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 3004.
060121*        10 FILLER                 PIC X(01)        VALUE 'J'.
060121         10 FILLER                 PIC X(01)        VALUE 'N'.
               10 FILLER                 PIC X(200)       VALUE SPACES.
               10 FILLER                 PIC S9(4) BINARY VALUE +051.
      * 3008   --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 3008.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)       VALUE SPACES.
               10 FILLER                 PIC S9(4) BINARY VALUE +051.
      * 4003   --------------------------------------------------------
      *        DIE PGV AUS DENEN DIE STRUKTUR ERMITTELT WERDEN DÜRFEN
      *        SIND DIREKT IM SQL(ZJPA0017) FUNKTION 51 ABGELEGT
      *        --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 4003.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)       VALUE SPACES.
               10 FILLER                 PIC S9(4) BINARY VALUE +051.
      * 4004   --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 4004.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)       VALUE SPACES.
               10 FILLER                 PIC S9(4) BINARY VALUE +051.
      * 4008   --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 4008.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)       VALUE SPACES.
               10 FILLER                 PIC S9(4) BINARY VALUE +051.
 
      *        >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      *        FUNKTION 52 OHNE VORBEWEGUNG, STRUKTUR AV
      *        >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      *
      * 1912   --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 1912.
               10 FILLER                 PIC X(01)        VALUE 'N'.
               10 FILLER                 PIC X(200)
                  VALUE '4912,4919,4989'.
               10 FILLER                 PIC S9(4) BINARY VALUE +052.
 
      *        >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      *        FUNKTION 53 STRUKTUR PAUSCHAL
      *        >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      *
      * 3086   --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 3086.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)
                  VALUE '2086'.
               10 FILLER                 PIC S9(4) BINARY VALUE +053.
      * 3087   --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 3087.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)
                  VALUE '2087'.
               10 FILLER                 PIC S9(4) BINARY VALUE +053.
 
      *        >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      *        FUNKTION 53 STRUKTUR AV
      *        >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      *
      * 3958   --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 3958.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)
                  VALUE '2913,2958'.
               10 FILLER                 PIC S9(4) BINARY VALUE +053.
      * 3980   --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 3980.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)
                  VALUE '2905,2907,2980'.
               10 FILLER                 PIC S9(4) BINARY VALUE +053.
      * 3982   --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 3982.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)
                  VALUE '2925,2982'.
               10 FILLER                 PIC S9(4) BINARY VALUE +053.
      * 3983   --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 3983.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)
                  VALUE '2935,2983'.
               10 FILLER                 PIC S9(4) BINARY VALUE +053.
      * 3984   --------------------------------------------------------
      *        WIK ÄNDERUNGSZUGANG (RIESTER ERHÖHUNG VERTEILT AUF 10J.)
      *        --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 3984.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)
                  VALUE '2945,2984'.
               10 FILLER                 PIC S9(4) BINARY VALUE +053.
      * 3988   --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 3988.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)
                  VALUE '2908,2988'.
               10 FILLER                 PIC S9(4) BINARY VALUE +053.
      * 3989   --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 3989.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)
                  VALUE '2919,2989'.
               10 FILLER                 PIC S9(4) BINARY VALUE +053.
      * 3990   --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 3990.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)
                  VALUE '2900,2902,2990'.
               10 FILLER                 PIC S9(4) BINARY VALUE +053.
      * 3992   --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 3992.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)
                  VALUE '2920,2992'.
               10 FILLER                 PIC S9(4) BINARY VALUE +053.
      * 3998  --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 3998.
               10 FILLER                 PIC X(01)        VALUE 'J'.
               10 FILLER                 PIC X(200)
                  VALUE '2903,2998'.
               10 FILLER                 PIC S9(4) BINARY VALUE +053.
 
      *        >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      *        FUNKTION 54
      *        >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      *
      * 1920   --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 1920.
               10 FILLER                 PIC X(01)        VALUE 'N'.
               10 FILLER                 PIC X(200)
                  VALUE '1005,4005,4920,4925'.
               10 FILLER                 PIC S9(4) BINARY VALUE +054.
 
      *        >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      *        FUNKTION 55
      *        >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      *
      * 1930   --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 1930.
               10 FILLER                 PIC X(01)        VALUE 'N'.
               10 FILLER                 PIC X(200)
                  VALUE '1005,4005,4935'.
               10 FILLER                 PIC S9(4) BINARY VALUE +055.
 
      *        >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      *        FUNKTION 56
      *        >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      *
      * 1902   --------------------------------------------------------
               10 FILLER                 PIC S9(4) BINARY VALUE 1902.
               10 FILLER                 PIC X(01)        VALUE 'N'.
               10 FILLER                 PIC X(200)       VALUE SPACES.
               10 FILLER                 PIC S9(4) BINARY VALUE +056.
      *        --------------------------------------------------------
      *        --------------------------------------------------------
 
 
 
 
 
 
 
 
 
 
 
 

