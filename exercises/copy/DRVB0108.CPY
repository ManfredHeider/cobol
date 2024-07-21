      ****************************************************************
      *    COPY-MEMBER: DRVB0108, SCHNITTSTELLE UPVB0093             *
      *                 ERM. KORRIGIERTE EINHEITEN UND UNTST-INFOS   *
      *    --------------------------------------------------------  *
      *    ERSTELLT: W. SCHÖLLHAMMER                                 *
      *    DATUM:    09.09.1996                                      *
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFVB0303!09.09.96!    ! NEU                       ! SCHÖLLH. *
      * AVB00544!06.11.01! 2.1! Umbenennung -40-PROZ... in! Walb     *
      *         !        !    ! -ABZ-PROZ...              !          *
      * AVB00649!03.12.02! 2.2! Dim 7ukunft: neues Auf-   ! Walb     *
      *         !        !    ! stiegssystem für RGS - D  !          *
      * AVB00764!01.08.05! 2.3! Neues Feld für Einheiten  ! Walb     *
      *         !        !    ! aus beantr. Betreuerwechsel          *
      *         !        !    ! neue Felder für Anzahl    !          *
      *         !        !    ! Partner                   !          *
      *         !26.04.06! 2.4! Neue Felder für Partner   ! Walb     *
      * AVB00866!14.07.06!2.5 ! Neues Feld für Vorausschau! Walb     *
      * FVB00659!05.04.07!2.6 ! Unterst-Tabelle erweitern ! Walb     *
      * FVB00677!15.06.07!2.7 ! Unterst-Tabelle erweitern ! Walb     *
      * AVB00923!02.07.07!2.8 ! Neues Feld für Anzahl Er- ! Walb     *
      *         !        !    ! satzpartner               !          *
      * AVB00996!16.09.08!2.9 ! Neue Felder für Allfinanz ! Walb     *
      *         !        !    ! mit Überleitungsvereinb.  !          *
      * AVB01036!06.12.10!2.10! Neues Feld für 'befördert ! Walb     *
      *         !        !    ! am' für Betreuer          !          *
      * AVB01409!07.07.16!2.11! Neues Feld für Datum des  ! Walb     *
      *         !        !    ! Betreuerwechsels          !          *
      * ASL00573!10.01.18!2.12! Neues Feld für umsatz-    ! Walb     *
      *         !        !    ! steuerfreie Berechnung    !          *
      * FVB00826!28.08.19!2.13! INDEX I-UNTST 120 > 150   ! BOLL     *
      * FVB00831!29.10.19!2.41! ZÄHLER I-UNTST-UEBERLAUF  !TH.WILLIG *
      *         !        !    ! HOCH GESETZT AUF '200'    !          *
      * AVB01632!24.06.19!2.42! ZÄHLER I-UNTST-UEBERLAUF  !TH.WILLIG *
      *         !        !    ! HOCH GESETZT AUF '300'    !          *
120722* VLA-5045!08.07.22!2.43! Num. gepackte Felder      ! HEIM     *
091222* VLA-5721!09.12.22!2.44! Num. gepackte Felder      ! HEIM     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      ***************************************************************
      *                 PARAMETER-INPUT                             *
      ***************************************************************
       01  PI-UPVB0093.
           05  PI-UPVB0093-FKT                PIC  X(003).
               88 PI-VB93-FKT-KORR-EINHEITEN            VALUE 'KEH'.

           05  PI-UPVB0093-BETR-NR            PIC S9(009) BINARY.
      *    * MS-NR-NEU MUSS MINDESTENS STUFE RGS SEIN
           05  PI-UPVB0093-BETR-MS-NR-NEU     PIC S9(004) BINARY.
      *    * BETRACHTUNGS-ZEITRAUM
      *    *  -VON: 1. TAG DES ERSTEN GÜLTIGEN STATISTIKMONATS
      *    *  -BIS: LETZTER TAG DES LETZTEN GÜLTIGEN STATISTIKMONATS
      *    *        (ALSO NICHT DAS TAGESDATUM)
           05  PI-UPVB0093-ZEITRAUM-VON-ISO   PIC  X(010).
           05  PI-UPVB0093-ZEITRAUM-BIS-ISO   PIC  X(010).
           05  PI-UPVB0093-KNR-GESELL-TYP     PIC S9(004) COMP.
           05  PI-UPVB0093-KNR-BEFBED-TYP     PIC S9(004) COMP.
           05  PI-UPVB0093-VORAUSSCHAU-MONATE PIC S9(004) COMP.
           05  PI-UPVB0093-S-ALLFINANZ-UV     PIC  X(001).
           05  PI-UPVB0093-BETR-BEF-DAT-ISO   PIC  X(010).
           05  PI-UPVB0093-S-UMSATZST-FREI    PIC  X(001).
      *
      ***************************************************************
      *                 PARAMETER-OUTPUT                            *
      ***************************************************************
       01  PO-UPVB0093.

      *    * ERGEBNIS:
           05  PO-UPVB0093-RC                 PIC  X(003).
               88  PO-VB93-RC-OK                           VALUE 'OK '.

      ****** INFORMATIONEN ZU BETREUER

           05  PO-UPVB0093-BETR-INFOS.
      *        * GRUPPEN-EINHEITEN
               10  PO-UPVB0093-EH-GS-NT-GR-SOLL   PIC S9(009)V9 COMP-3.
               10  PO-UPVB0093-EH-ABZ-PROZ-GR     PIC S9(009)V9 COMP-3.
               10  PO-UPVB0093-EH-GS-NT-GR-IST    PIC S9(009)V9 COMP-3.
               10  PO-UPVB0093-EH-GS-NT-GR-KORR   PIC S9(009)V9 COMP-3.
      *        * GESAMT-EINHEITEN
               10  PO-UPVB0093-EH-GS-NT-GES-SOLL  PIC S9(009)V9 COMP-3.
               10  PO-UPVB0093-EH-ABZ-PROZ-GES    PIC S9(009)V9 COMP-3.
               10  PO-UPVB0093-EH-GS-NT-GES-IST   PIC S9(009)V9 COMP-3.
               10  PO-UPVB0093-EH-GS-NT-GES-KORR  PIC S9(009)V9 COMP-3.
      *        * GESAMT-EINHEITEN Netto-Neu
               10  PO-UPVB0093-EH-GS-NT-GES-NEU   PIC S9(009)V9 COMP-3.
      
      ****** SUMMEN ÜBER ALLE UNTERSTELLTE (KENNUNG -2-)
      
           05  PO-UPVB0093-2-UNTST-SUMMEN.
      *        * SUMME: GESAMT-NETTO-EINHEITEN OHNE ZUSCHLÄGE
      *        *   I.D.R. NEU- UND FOLGEGESCHÄFT;
               10  PO-UPVB0093-2-EH-GS-NT-GES-IST PIC S9(009)V9 COMP-3.
               10  PO-UPVB0093-2-EH-ZUSCHLAG      PIC S9(009)V9 COMP-3.
               10  PO-UPVB0093-2-EH-ABZUG         PIC S9(009)V9 COMP-3.
      *        * ANZAHL UNTERSTELLTE AB VBA ZUM AKT. BETREUER
               10  PO-UPVB0093-2-UNTST-ANZ        PIC S9(004)   COMP.
               10  PO-UPVB0093-2-ANZ-PARTNER      PIC S9(004)   COMP.
               10  PO-UPVB0093-2-ANZ-HPTB-PARTNER PIC S9(004)   COMP.
               10  PO-UPVB0093-2-ANZ-HB-PARTN-ERS PIC S9(004)   COMP.
               10  PO-UPVB0093-2-ANZ-PART-AB-RGS  PIC S9(004)   COMP.

      ****** TABELLE: INFOS UND EINHEITEN JE UNTERSTELLTEM (KENNUNG -3-)

           05  PO-UPVB0093-3-UNTST-INFOS OCCURS 300 TIMES
                                         INDEXED BY I-UNTST.
               10  PO-UPVB0093-3-UNTST-NR         PIC S9(009) BINARY.

      *        * EREIGNISSE ZU BEGINN UND AM ENDE DER BETREUUNGSPHASE
               10  PO-UPVB0093-3-EREIGNIS-ANFANG  PIC  X(002).
               10  PO-UPVB0093-3-EREIGNIS-ENDE    PIC  X(002).

      *        * BERECHNUNGS-ZEITRAUM (ALLE im FormatT !!!)
      *        *   -BIS    = BIS OHNE VERLÄNGERUNG
      *        *   -BVL    = BIS MIT VERLÄNGERUNG NACH LBB 3.4
      *        *             (IMMER GEFÜLLT, I.D.R. IDENTISCH MIT -BIS)
               10  PO-UPVB0093-3-BER-ZEITR-VON.
                   15  PO-UPVB0093-3-BER-ZEITR-VON-MM PIC  X(002).
                   15  PO-UPVB0093-3-BER-ZEITR-VON-J4 PIC  X(004).
               10  PO-UPVB0093-3-BER-ZEITR-BIS.
                   15  PO-UPVB0093-3-BER-ZEITR-BIS-MM PIC  X(002).
                   15  PO-UPVB0093-3-BER-ZEITR-BIS-J4 PIC  X(004).
               10  PO-UPVB0093-3-BER-ZEITR-BVL.
                   15  PO-UPVB0093-3-BER-ZEITR-BVL-MM PIC  X(002).
                   15  PO-UPVB0093-3-BER-ZEITR-BVL-J4 PIC  X(004).

      *        * BASIS-EINHEITEN (OHNE ZUSCHLÄGE WG. LBB 3.4)
091222           10  PO-UPVB0093-3-EH-GS-NT-GES-IST PIC S9(09)V9 COMP-3.
091222           10  PO-UPVB0093-3-EH-ZUSCHLAG      PIC S9(09)V9 COMP-3.
091222           10  PO-UPVB0093-3-EH-ABZUG         PIC S9(09)V9 COMP-3.
091222           10  PO-UPVB0093-3-EH-ABZUG-BW      PIC S9(09)V9 COMP-3.

      *        * J-/N-SCHALTER = ZUSATZINFOS ZU DEN EINHEITEN
      *        * 1. BEINHALTEN DIE BASISEINHEITEN UND DIE ZUSCHLÄGE
      *        *    NUR NEUGESCHÄFT ?
      *        * 2. ERFOLGT DER ABZUG WG. DER 40-PROZENT-KLAUSEL ?
      *        *    ( BEI 'J': 100% = PO-UPVB0093-EH-40-PROZ-GR /-GES
      *        *               EINHEITEN DES UNTST DIE GEPRÜFT WERDEN:
      *        *               BASISEINHEITEN + ZUSCHLAG              )
      *        *    ( BEI 'N': ABZUG WG. LBB 3.5)
               10  PO-UPVB0093-3-S-EH-NUR-NEUG    PIC  X(001).
               10  PO-UPVB0093-3-S-EH-ABZ-WG-ABZR PIC  X(001).
               10  PO-UPVB0093-3-S-EH-ABZ-WG-BW   PIC  X(001).
      *        * gilt für Partnerregelung (neue Stufe = RGS und > RGS)
               10  PO-UPVB0093-3-S-PARTNER-WERTEN PIC  X(001).
      *        * nur gefüllt für neue Stufe > RGS
               10  PO-UPVB0093-3-EH-PARTNER       PIC S9(009)V9 COMP-3.
               10  PO-UPVB0093-3-PART-DAT-VON.
                   15  PO-UPVB0093-3-PART-DAT-VON-MM PIC  X(002).
                   15  PO-UPVB0093-3-PART-DAT-VON-J4 PIC  X(004).
               10  PO-UPVB0093-3-PART-DAT-BIS.
                   15  PO-UPVB0093-3-PART-DAT-BIS-MM PIC  X(002).
                   15  PO-UPVB0093-3-PART-DAT-BIS-J4 PIC  X(004).
      *        * gilt nur für Allfinanz mit Überleitungsvereinbarung
               10  PO-UPVB0093-3-S-PART-WERT-RGS  PIC  X(001).
      *        * nur gefüllt bei beantragtem BW oder BW durch Überholen
               10  PO-UPVB0093-3-BW-DATUM-ISO     PIC  X(010).

      ******** ENDE COPY-MEMBER   DRVB0108   ***********************






