      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  27.05.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0714.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRVB0714.CPv  $
      *
      *   Rev 2.3   May 15 2006 13:42:32   t13ex43
      *ASL00226: PGM5 - Dimension 7 Zukunft - MAP
      *
      *   Rev 2.2   Apr 18 2006 10:39:56   t13ex43
      *ASL00226: PGM - Dimension 7 Zukunft - MAP
      *
      *   Rev 2.1   Jul 14 2004 09:31:48   t13ex43
      *ASL00183: PGM - Ermittlung MAP-Kandidaten
      *
      *   Rev 2.0   May 27 2004 12:22:14   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVB0714, SCHNITTSTELLE UPVB0137             *
      *                 ERM. KORRIGIERTE EINHEITEN UND UNTST-INFOS   *
      *    --------------------------------------------------------  *
      *    ERSTELLT: KLEEMEIER                                       *
      *    DATUM:    27.05.2004                                      *
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ASL00183!27.05.04!2.1 ! NEU (ANALOG DRVB0566, MIT ! KLEEMEIER*
      *         !        !    ! VARIABLER %-KLAUSEL)      !          *
200306* AVB00764!20.03.06!2.2 ! ÄNDERUNGEN AUS VORLAGE    ! KLEEMEIER*
200306*         !        !    ! DRVB0108 ÜBERNOMMEN       !          *
260406* ASL00226!12.05.06!2.3 ! ÄNDERUNGEN AUS VORLAGE    ! KLEEMEIER*
260406*         !26.04.06!    ! NEUE FELDER FÜR PARTNER   !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      ***************************************************************
      *                 PARAMETER-INPUT                             *
      ***************************************************************
       01  PI-UPVB0137.
           05  PI-UPVB0137-FKT                PIC  X(003).
               88 PI-VB93-FKT-KORR-EINHEITEN            VALUE 'KEH'.

           05  PI-UPVB0137-PROZ-KLAUSEL       PIC S9(003)V99 COMP-3.

           05  PI-UPVB0137-KNR-GESELL-TYP     PIC S9(004) COMP.
           05  PI-UPVB0137-BETR-NR            PIC S9(009) BINARY.
      *    * MS-NR-NEU MUSS MINDESTENS STUFE RGS SEIN
           05  PI-UPVB0137-BETR-MS-NR-NEU     PIC S9(004) BINARY.
      *    * BETRACHTUNGS-ZEITRAUM
      *    *  -VON: 1. TAG DES ERSTEN GÜLTIGEN STATISTIKMONATS
      *    *  -BIS: LETZTER TAG DES LETZTEN GÜLTIGEN STATISTIKMONATS
      *    *        (ALSO NICHT DAS TAGESDATUM)
           05  PI-UPVB0137-ZEITRAUM-VON-ISO   PIC  X(010).
           05  PI-UPVB0137-ZEITRAUM-BIS-ISO   PIC  X(010).
      *
      ***************************************************************
      *                 PARAMETER-OUTPUT                            *
      ***************************************************************
       01  PO-UPVB0137.

      *    * ERGEBNIS:
           05  PO-UPVB0137-RC                 PIC  X(003).
               88  PO-VB93-RC-OK                           VALUE 'OK '.

      ****** INFORMATIONEN ZU BETREUER

           05  PO-UPVB0137-BETR-INFOS.
      *        * GRUPPEN-EINHEITEN
               10  PO-UPVB0137-EH-GS-NT-GR-SOLL   PIC S9(009)V9 COMP-3.
               10  PO-UPVB0137-EH-ABZ-PROZ-GR     PIC S9(009)V9 COMP-3.
               10  PO-UPVB0137-EH-GS-NT-GR-IST    PIC S9(009)V9 COMP-3.
               10  PO-UPVB0137-EH-GS-NT-GR-KORR   PIC S9(009)V9 COMP-3.
      *        * GESAMT-EINHEITEN
               10  PO-UPVB0137-EH-GS-NT-GES-SOLL  PIC S9(009)V9 COMP-3.
               10  PO-UPVB0137-EH-ABZ-PROZ-GES    PIC S9(009)V9 COMP-3.
               10  PO-UPVB0137-EH-GS-NT-GES-IST   PIC S9(009)V9 COMP-3.
               10  PO-UPVB0137-EH-GS-NT-GES-KORR  PIC S9(009)V9 COMP-3.

      ****** SUMMEN ÜBER ALLE UNTERSTELLTE (KENNUNG -2-)

           05  PO-UPVB0137-2-UNTST-SUMMEN.
      *        * SUMME: GESAMT-NETTO-EINHEITEN OHNE ZUSCHLÄGE
      *        *   I.D.R. NEU- UND FOLGEGESCHÄFT;
               10  PO-UPVB0137-2-EH-GS-NT-GES-IST PIC S9(009)V9 COMP-3.
               10  PO-UPVB0137-2-EH-ZUSCHLAG      PIC S9(009)V9 COMP-3.
               10  PO-UPVB0137-2-EH-ABZUG         PIC S9(009)V9 COMP-3.
      *        * ANZAHL UNTERSTELLTE AB VBA ZUM AKT. BETREUER
               10  PO-UPVB0137-2-UNTST-ANZ        PIC S9(004)   COMP.
200306         10  PO-UPVB0137-2-ANZ-PARTNER      PIC S9(004)   COMP.
200306         10  PO-UPVB0137-2-ANZ-HPTB-PARTNER PIC S9(004)   COMP.

      ****** TABELLE: INFOS UND EINHEITEN JE UNTERSTELLTEM (KENNUNG -3-)

           05  PO-UPVB0137-3-UNTST-INFOS OCCURS 100
                                         INDEXED BY I-UNTST.

               10  PO-UPVB0137-3-UNTST-NR         PIC S9(009) BINARY.

      *        * EREIGNISSE ZU BEGINN UND AM ENDE DER BETREUUNGSPHASE
               10  PO-UPVB0137-3-EREIGNIS-ANFANG  PIC  X(002).
               10  PO-UPVB0137-3-EREIGNIS-ENDE    PIC  X(002).

      *        * BERECHNUNGS-ZEITRAUM (ALLE IM FORMATT !!!)
      *        *   -BIS    = BIS OHNE VERLÄNGERUNG
      *        *   -BVL    = BIS MIT VERLÄNGERUNG NACH LBB 3.4
      *        *             (IMMER GEFÜLLT, I.D.R. IDENTISCH MIT -BIS)
               10  PO-UPVB0137-3-BER-ZEITR-VON.
                   15  PO-UPVB0137-3-BER-ZEITR-VON-MM PIC  X(002).
                   15  PO-UPVB0137-3-BER-ZEITR-VON-J4 PIC  X(004).
               10  PO-UPVB0137-3-BER-ZEITR-BIS.
                   15  PO-UPVB0137-3-BER-ZEITR-BIS-MM PIC  X(002).
                   15  PO-UPVB0137-3-BER-ZEITR-BIS-J4 PIC  X(004).
               10  PO-UPVB0137-3-BER-ZEITR-BVL.
                   15  PO-UPVB0137-3-BER-ZEITR-BVL-MM PIC  X(002).
                   15  PO-UPVB0137-3-BER-ZEITR-BVL-J4 PIC  X(004).

      *        * BASIS-EINHEITEN (OHNE ZUSCHLÄGE WG. LBB 3.4)
               10  PO-UPVB0137-3-EH-GS-NT-GES-IST PIC S9(009)V9 COMP-3.

               10  PO-UPVB0137-3-EH-ZUSCHLAG      PIC S9(009)V9 COMP-3.
               10  PO-UPVB0137-3-EH-ABZUG         PIC S9(009)V9 COMP-3.
200306         10  PO-UPVB0137-3-EH-ABZUG-BW      PIC S9(009)V9 COMP-3.

      *        * J-/N-SCHALTER = ZUSATZINFOS ZU DEN EINHEITEN
      *        * 1. BEINHALTEN DIE BASISEINHEITEN UND DIE ZUSCHLÄGE
      *        *    NUR NEUGESCHÄFT ?
      *        * 2. ERFOLGT DER ABZUG WG. DER PROZENT-KLAUSEL ?
      *        *    ( BEI 'J': 100% = PO-UPVB0137-EH-ABZ-PROZ-GR /-GES
      *        *               EINHEITEN DES UNTST DIE GEPRÜFT WERDEN:
      *        *               BASISEINHEITEN + ZUSCHLAG              )
      *        *    ( BEI 'N': ABZUG WG. LBB 3.5)
               10  PO-UPVB0137-3-S-EH-NUR-NEUG    PIC  X(001).
               10  PO-UPVB0137-3-S-EH-ABZ-WG-ABZR PIC  X(001).
200306         10  PO-UPVB0137-3-S-EH-ABZ-WG-BW   PIC  X(001).
260406*        * GILT FÜR PARTNERREGELUNG (NEUE STUFE = RGS UND > RGS)
260406         10  PO-UPVB0093-3-S-PARTNER-WERTEN PIC  X(001).
260406*        * NUR GEFÜLLT FÜR NEUE STUFE > RGS
260406         10  PO-UPVB0093-3-EH-PARTNER       PIC S9(009)V9 COMP-3.
260406         10  PO-UPVB0093-3-PART-DAT-VON.
260406             15  PO-UPVB0093-3-PART-DAT-VON-MM PIC  X(002).
260406             15  PO-UPVB0093-3-PART-DAT-VON-J4 PIC  X(004).
260406         10  PO-UPVB0093-3-PART-DAT-BIS.
260406             15  PO-UPVB0093-3-PART-DAT-BIS-MM PIC  X(002).
260406             15  PO-UPVB0093-3-PART-DAT-BIS-J4 PIC  X(004).
      *
      ******** ENDE COPY-MEMBER   DRVB0714   ***********************
      *
