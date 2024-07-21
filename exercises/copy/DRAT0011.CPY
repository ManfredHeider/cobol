      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0011.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRAT0011.CPv  $
      *
      *   Rev 2.1   Apr 06 1999 11:33:14   T13AE34
      *AAT00005-Euro-Swicht
      *
      *   Rev 2.0   Mar 01 1999 14:44:38   T13AE34
      *NT-Umstellung
      *
      *   Rev 1.2   Mar 07 1996 14:46:46   T13AE17
      *AFAN0096   Anpassung WB
      *
      *   Rev 1.1   Nov 07 1995 16:06:40   T13AE25
      *AFVB0226: Neue Felder für Füllen variabler
      *          Meldungs-Felder
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: ZWISCHENFELDER        (DRAT0011)              *
      *                 - PROGRAMMRAHMEN -                            *
      *    --------------------------------------------------------   *
      *    ERSTELLT: W. SCHOELLHAMMER (INTEGRATA AG)                  *
      *    DATUM:    12.01.1993                                       *
      *****************************************************************
      *    VERSION   2    26.08.94 16:33:12
      *****************************************************************
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !12.01.93! 01 ! NEU                       ! SCHÖLLH. *
      * AFRZ0013!26.08.94! 02 ! UMSTELLUNG KUS-SYSTEM AUF ! WIECHERS *
      *         !        !    !  RACF                     !          *
      * -------------------------------------------------------------*
      * AFVB0226!28.09.95!    ! NEUE FELDER FÜR FÜLLEN    ! SCHÖLLH. *
      *         !        !    !  VARIABLER MELDUNGS-FELDER!          *
      *         !07.11.95! 03 ! PROD.UEBERGABE            ! SCHÖLLH. *
      * -------------------------------------------------------------*
      * ZUS. MIT!        !    ! AENDERUNG FUER WORKBENCH  ! KÜKEN.   *
      * AFAN0096!07.03.96! 04 !                           !          *
      * -------------------------------------------------------------*
      * AAT00005!17.02.99!1.3 ! EURO-SWITCH IN NEUDIALOGEN! HEIDER   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                               *
      *    ENTHAELT:
      *       Z-ERROR-ZWISCHENFELDER   (FUER ERROR UND TRACE IM RAHMEN)
      *       Z-DIALOG-ZWISCHENFELDER  (SONSTIGE ZWISCHENFELDER IM R.)
      *
       01  Z-DIALOG-ZWISCHENFELDER.

      *   BILDSCHIRMEINGABE-ZWISCHENBEREICH
           05  Z-EINBER                   PIC  X(1920).
           05  Z-EINBER-LEN               PIC S9(004) BINARY VALUE 1920.

           05  Z-TSQ-NAME                 PIC  X(008).
           05  Z-TSQ-ITEM                 PIC S9(004) BINARY.

170299*   EURO-SWITCH BILDSCHIRMEINGABE-ZWISCHENBEREICH
           05  Z-EURO-SWITCH-EINBER       PIC  X(1920).
           05  Z-EURO-SWITCH-EINBER-LEN   PIC S9(004) BINARY VALUE 1920.

170299*   WELCHE AKTION WIRD BEIM EURO-SWITCH AUSGEFüHRT
           05 Z-EURO-SWITCH-AKTION             PIC S9(0004) BINARY.
                88 Z-EURO-SWITCH-ALLES           VALUE 1.
                88 Z-EURO-SWITCH-NUR-TEXTE       VALUE 2.
                88 Z-EURO-SWITCH-NUR-BETRAEGE    VALUE 3.

170299*   ARBEITSFELDER FÜR EURO-SWITCH MASKENVERGLEICH
170299     05  Z-EURO-SWITCH-ZEICHEN PIC  X(0001).
170299         88  DARSTELLBARES-ZEICHEN  VALUE 'a' THRU 'z'
                                                'ä' 'ö' 'ü'
                                                'A' THRU 'Z'
                                                'Ä' 'Ö' 'Ü'
                                                '0'
                                                '1' THRU '9'
                   ','  '.' ';' ':' '^' '"' '§' '$' '%' '&' '/' '('
                   ')' '=' '?' '{' '[' ']' '}' '\' '@' '+' '*' '~'
                   '#' '<' '>' '|' '_' '-'.
170299*
           05 I-EURO-SWITCH                    PIC S9(0004) BINARY.
      *
           05  Z-MAP-NEXT                 PIC  X(007).
260894*     05  Z-PERSNR                   PIC  9(007).
260894     05  Z-USERID                   PIC  X(007).
           05  Z-OP-TAB-FUELLWERT         PIC  X(003).
           05  Z-FTASTE                   PIC  X(001).
           05  Z-BFANZ-NUM                PIC  9(002).
      *    * AKTUELLES DATUM (FORM: JJJJMMTT)
           05  Z-AKT-DATUM-AN.
               10  Z-AKT-JAHR             PIC  9(004).
               10  Z-AKT-JAHR-AN REDEFINES Z-AKT-JAHR.
                   15  Z-AKT-JH           PIC  9(002).
                   15  Z-AKT-JJ           PIC  9(002).
               10  Z-AKT-MONAT            PIC  9(002).
               10  Z-AKT-TAG              PIC  9(002).
           05  Z-AKT-DATUM-RECH REDEFINES Z-AKT-DATUM-AN
                                          PIC 9(008).

      *    * ZWISCHENFELD FUER FORMATTIME
           05  Z-AKT-DATUM-TEMP.
               10  Z-AKT-JJ-TEMP          PIC  9(002).
               10  FILLER                 PIC  X(001).
               10  Z-AKT-MONAT-TEMP       PIC  9(002).
               10  FILLER                 PIC  X(001).
               10  Z-AKT-TAG-TEMP         PIC  9(002).

      *    * ABSOLUT-ZEIT UND YEAR FUER CICS-AUFRUF
070396*    05  Z-ABSZEIT                  PIC  X(008).
070396     05  Z-ABSZEIT                  PIC S9(015) PACKED-DECIMAL.
           05  Z-YEAR                     PIC S9(009) BINARY.

280995*    * ZWISCHENFELDER FÜR FÜLLEN VARIABLE TEXTE IN FEHLERMELDUNGEN
280995     05  Z-VT-ZAEHLER               PIC S9(004) COMP.
280995*        * ZWISCHENFELDER FÜR UNSTRING FEHLERMELDUNGS-TEXT
280995     05  Z-VT-FELDER.
280995         10  Z-VT-FELD1             PIC  X(080).
280995         10  Z-VT-FELD2             PIC  X(080).
280995         10  Z-VT-FELD3             PIC  X(080).
280995         10  Z-VT-FELD4             PIC  X(080).


       01  Z-ERROR-ZWISCHENFELDER.

      *       PARAMETER FUER DISPLAY VON TRACE-INFOS
           05  Z-TRACE-DISPLAY-LINE       PIC S9(004) BINARY VALUE ZERO.
           05  Z-TRACE-DISPLAY-LEN        PIC S9(004) BINARY VALUE ZERO.

      *       (VMTL. OBSOLET; 29.3.93)
           05  Z-ERR-RC-ZEILE.
               10  FILLER                 PIC  X(008) VALUE 'ERR-RC: '.
               10  Z-ERR-RC               PIC  9(001).

      *
      ******** ENDE COPY-MEMBER ZWISCHENFELDER (DRAT0011) *************
