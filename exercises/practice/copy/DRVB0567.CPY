      ****************************************************************
      *    COPY-MEMBER: DRVB0567, SCHNITTSTELLE UPVB0093             *
      *                 ERM. KORRIGIERTE EINHEITEN UND UNTST-INFOS   *
      *    --------------------------------------------------------  *
      *    ERSTELLT: W. SCH™LLHAMMER                                 *
      *    DATUM:    09.09.1996                                      *
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ASL00105!07.02.02! 1  ! Kopie DRVB0108 für VSW2001! D.Ehrmann*
      *         !        !    ! und fix 40%.              !          *
      * ASL00309!04.08.08! 2.2! OCCURS ERHÖHT (UNTST-INFO)! KLEEMEIER*
      *         !        !    ! RC UMBENANNT (VB93-RC-OK  !          *
      *         !        !    ! GIBT ES AUCH IN DRVB0108, !          *
      *         !        !    ! DITO VB93-FKT-KORR-..)    !          *
081122* VLA-5636!08.11.22! 3.0! Num. gepackte Felder  -   ! HEIM     *
091222* VLA-5721!09.12.22! 3.1! Num. gepackte Felder  -   ! HEIM     *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      ***************************************************************
      *                 PARAMETER-INPUT                             *
      ***************************************************************
       01  PI-UPVB0122.
           05  PI-UPVB0122-FKT                PIC  X(003).
               88 PI-UPVB0122-FKT-KORR-EINHEITEN   VALUE 'KEH'.
      
           05  PI-UPVB0122-BETR-NR            PIC S9(009) BINARY.
      *    * MS-NR-NEU MUSS MINDESTENS STUFE RGS SEIN
           05  PI-UPVB0122-BETR-MS-NR-NEU     PIC S9(004) BINARY.
      *    * BETRACHTUNGS-ZEITRAUM
      *    *  -VON: 1. TAG DES ERSTEN GšLTIGEN STATISTIKMONATS
      *    *  -BIS: LETZTER TAG DES LETZTEN GšLTIGEN STATISTIKMONATS
      *    *        (ALSO NICHT DAS TAGESDATUM)
           05  PI-UPVB0122-ZEITRAUM-VON-ISO   PIC  X(010).
           05  PI-UPVB0122-ZEITRAUM-BIS-ISO   PIC  X(010).
      *
      ***************************************************************
      *                 PARAMETER-OUTPUT                            *
      ***************************************************************
       01  PO-UPVB0122.
      
      *    * ERGEBNIS:
           05  PO-UPVB0122-RC                 PIC  X(003).
               88  PO-UPVB0122-RC-OK                       VALUE 'OK '.

      ****** INFORMATIONEN ZU BETREUER

           05  PO-UPVB0122-BETR-INFOS.
      *        * GRUPPEN-EINHEITEN
               10  PO-UPVB0122-EH-GS-NT-GR-SOLL   PIC S9(009)V9 COMP-3.
               10  PO-UPVB0122-EH-ABZ-PROZ-GR     PIC S9(009)V9 COMP-3.
               10  PO-UPVB0122-EH-GS-NT-GR-IST    PIC S9(009)V9 COMP-3.
               10  PO-UPVB0122-EH-GS-NT-GR-KORR   PIC S9(009)V9 COMP-3.
      *        * GESAMT-EINHEITEN
               10  PO-UPVB0122-EH-GS-NT-GES-SOLL  PIC S9(009)V9 COMP-3.
               10  PO-UPVB0122-EH-ABZ-PROZ-GES    PIC S9(009)V9 COMP-3.
               10  PO-UPVB0122-EH-GS-NT-GES-IST   PIC S9(009)V9 COMP-3.
               10  PO-UPVB0122-EH-GS-NT-GES-KORR  PIC S9(009)V9 COMP-3.
      
      ****** SUMMEN šBER ALLE UNTERSTELLTE (KENNUNG -2-)
      
           05  PO-UPVB0122-2-UNTST-SUMMEN.
      *        * SUMME: GESAMT-NETTO-EINHEITEN OHNE ZUSCHLŽGE
      *        *   I.D.R. NEU- UND FOLGEGESCHŽFT;
               10  PO-UPVB0122-2-EH-GS-NT-GES-IST PIC S9(009)V9 COMP-3.
               10  PO-UPVB0122-2-EH-ZUSCHLAG      PIC S9(009)V9 COMP-3.
               10  PO-UPVB0122-2-EH-ABZUG         PIC S9(009)V9 COMP-3.
      *        * ANZAHL UNTERSTELLTE AB VBA ZUM AKT. BETREUER
               10  PO-UPVB0122-2-UNTST-ANZ        PIC S9(004)   COMP.
      
      ****** TABELLE: INFOS UND EINHEITEN JE UNTERSTELLTEM (KENNUNG -3-)
      
           05  PO-UPVB0122-3-UNTST-INFOS OCCURS 120 TIMES
                                         INDEXED BY I-UNTST.
               10  PO-UPVB0122-3-UNTST-NR         PIC S9(009) BINARY.

      *        * EREIGNISSE ZU BEGINN UND AM ENDE DER BETREUUNGSPHASE
               10  PO-UPVB0122-3-EREIGNIS-ANFANG  PIC  X(002).
               10  PO-UPVB0122-3-EREIGNIS-ENDE    PIC  X(002).

      *        * BERECHNUNGS-ZEITRAUM (ALLE im FormatT !!!)
      *        *   -BIS    = BIS OHNE VERLŽNGERUNG
      *        *   -BVL    = BIS MIT VERLŽNGERUNG NACH LBB 3.4
      *        *             (IMMER GEFšLLT, I.D.R. IDENTISCH MIT -BIS)
               10  PO-UPVB0122-3-BER-ZEITR-VON.
                   15  PO-UPVB0122-3-BER-ZEITR-VON-MM PIC  X(002).
                   15  PO-UPVB0122-3-BER-ZEITR-VON-J4 PIC  X(004).
               10  PO-UPVB0122-3-BER-ZEITR-BIS.
                   15  PO-UPVB0122-3-BER-ZEITR-BIS-MM PIC  X(002).
                   15  PO-UPVB0122-3-BER-ZEITR-BIS-J4 PIC  X(004).
               10  PO-UPVB0122-3-BER-ZEITR-BVL.
                   15  PO-UPVB0122-3-BER-ZEITR-BVL-MM PIC  X(002).
                   15  PO-UPVB0122-3-BER-ZEITR-BVL-J4 PIC  X(004).

      *        * BASIS-EINHEITEN (OHNE ZUSCHLŽGE WG. LBB 3.4)
               10  PO-UPVB0122-3-EH-GS-NT-GES-IST PIC S9(009)V9 COMP-3.
               10  PO-UPVB0122-3-EH-ZUSCHLAG      PIC S9(009)V9 COMP-3.
               10  PO-UPVB0122-3-EH-ABZUG         PIC S9(009)V9 COMP-3.

      *        * J-/N-SCHALTER = ZUSATZINFOS ZU DEN EINHEITEN
      *        * 1. BEINHALTEN DIE BASISEINHEITEN UND DIE ZUSCHLŽGE
      *        *    NUR NEUGESCHŽFT ?
      *        * 2. ERFOLGT DER ABZUG WG. DER 40-PROZENT-KLAUSEL ?
      *        *    ( BEI 'J': 100% = PO-UPVB0122-EH-40-PROZ-GR /-GES
      *        *               EINHEITEN DES UNTST DIE GEPRšFT WERDEN:
      *        *               BASISEINHEITEN + ZUSCHLAG              )
      *        *    ( BEI 'N': ABZUG WG. LBB 3.5)
               10  PO-UPVB0122-3-S-EH-NUR-NEUG    PIC  X(001).
               10  PO-UPVB0122-3-S-EH-ABZ-WG-ABZR PIC  X(001).

      ******** ENDE COPY-MEMBER   DRVB0567   ***********************


