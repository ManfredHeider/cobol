      ****************************************************************
      *    COPY-MEMBER: DRPA0201  KONSTANTEN FÜR VSTAT-TABELLE       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                           *
      *    DATUM:    23.01.1997                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DAISY 1 !23.01.97! 01 ! NEUERSTELLUNG             ! WOLFF    *
      * PEW-Rel.!        !    !                           !          *
      *         !17.03.97! 02 ! NEUE KONSTANTEN           ! WOLFF    *
      * AFPA0012!20.06.97!    ! NEUE KONSTANTEN           ! WOLFF    *
      * AFPA0012!29.06.97!    ! NEUE KONSTANTEN           ! WOLFF    *
      *         !06.11.97!    ! ANPASSUNG AN NAMENSKON-   ! MUND     *
      *         !        !    ! VENTIONEN                 !          *
      *         !10.11.97!    ! NEU: K-VTAB-PRINTE        ! WOLFF    *
      *         !29.01.98!    ! NEU: K-VTAB-MELDUNG       ! PARENT   *
      *         !29.01.98!    ! NEU: K-VTAB-PEST-ID       ! PARENT   *
      *         !29.01.98!    ! NEU: K-VTAB-AKZ           ! PARENT   *
      * PEW-REL.!09.06.98!    ! NEU:                      !          *
      * 2.0     !        !    !  K-VTAB-PEWA-VOR-VNR-KZ   !          *
      *         !        !    !  K-VTAB-KNR-PEWA-ZUSTAND  !          *
      *         !        !    !  K-VTAB-KNR-STORNO-KZ     !          *
      *         !        !    !  K-VTAB-KNR-RUECKMELD-STAT!          *
      *         !05.08.98!    ! NEU: K-VTAB-BUCHUNG       ! FOLTZ    *
      * APA00067!12.04.00!2.1 ! NEU: K-VTAB-DTA-BADENIA   ! STORCH   *
      * APA00090!08.07.00!2.2 ! NEU: K-VTAB-DTA-III       ! MUND     *
      * APA00112!25.10.00!2.3 ! NEU: K-VTAB-DTA-DIT       ! KLEEMEIER*
      * APA00116!09.11.00!2.4 ! NEU: K-VTAB-DTA-HYPO      ! KLEEMEIER*
      * APA00118!23.01.01!2.5 ! NEU: K-VTAB-DTA-BFG       ! MUND     *
      * APA00131!21.02.01!2.7 ! NEU: K-VTAB-DTA-COMMERZB. ! STORCH   *
      *         !28.02.01!2.8 ! NEU: K-VTAB-DTA FÜR DGI,  ! STORCH   *
      *         !        !    ! CENTRAL, AML-DL, BFG-BANK !          *
      * APA00135!09.04.01!2.9 ! NEU: K-VTAB-DTA-BAWAG     ! KLEEMEIER*
      * APA00133!21.06.01!2.10! NEU: K-VTAB-DTA-DREBA     ! STORCH   *
      *         !09.07.01!    ! NEU: K-VTAB-DTA-ADVOCARD  !          *
      *         !19.07.01!    ! NEU: K-VTAB-DTA-DREBA-CH  !          *
      *         !26.07.01!    ! NEU: K-VTAB-DTA-DMB       !          *
      * APA00167!21.11.01!2.30! NEU: K-VTAB-DTA-DMB-OEST  ! MUND     *
      * APA00167!21.11.01!2.30! NEU: K-VTAB-DTA-BANK-AUSTR! MUND     *
      * APA00167!30.11.01!2.31! NEU: K-VTAB-DTA-BANK-AUS-I! JÄGER    *
      * APA00172!15.01.02!2.32! NEU: K-VTAB-DTA-DWS       ! JÄGER    *
      * APA00172!15.01.02!2.32! NEU: K-VTAB-DTA-DB24      ! JÄGER    *
      * APA00206!19.06.02!2.14! NEU: 8 NEUE K-VTAB-DTA    ! KLEEMEIER*
      * APA00262!05.12.03!2.15! NEU: K-VTAB-DTA-DSL-BANK  ! WANG     *
      * APA00270!11.03.04!2.16! NEU: K-VTAB-DTA-GENERALI-B! SCHEIDEL *
      *         !        !    ! NEU: K-VTAB-DTA-GENERALI-I!          *
      * APA01284!29.11.11!2.17! NEU:K-VTAB-DTA-SONST-GES-A! JÄGER    *
      * APA01526!13.02.13!2.18! NEU:K-VTAB-DTA-AM-LEBEN   ! STORCH   *
      *         !        !    ! UND PENSIONSKASSE         !          *
      * APA01932!17.03.15!2.19! NEU: K-VTAB-DTA-DVM       ! STORCH   *
      * APA02169!24.11.15!2.20! NEU: K-VTAB-DTA-CKV-VP    !KÖPRÜCÜ   *
      * APA02275!15.02.17!2.21! NEU: K-VTAB-FLEX-NX       ! STORCH   *
      *         !        !    ! K-VTAB FÜR TELESON        !          *
      * APA02360!24.04.17!2.22! NEU: K-VTAB FÜR FINGERHAUS! WANG     *
      * APA02438!24.04.17!    ! NEU: K-VTAB FÜR DEVOLO    ! WANG     *
      * APA02396!24.04.17!    ! NEU: K-VTAB FÜR LOHNWERK  ! WANG     *
      * APA02504!10.08.17!2.24! NEU: K-VTAB-T-NUMMER      ! STORCH   *
      * APA02473!23.08.17!    ! NEU: K-VTAB FÜR CADOOZ    ! STORCH   *
      * APA02498!18.10.17!2.25! NEU: K-VTAB FÜR CAPITAL   ! BUCH     *
      * APA02520!14.11.17!2.26! NEU: K-VTAB FÜR GEIGER    ! WANG     *
      * APA02579!22.03.18!2.27! NEU: K-VTAB FÜR ALAG      ! WANG     *
      *         !18.04.18!2.28! ERGÄNZUNGEN FÜR ALAG      ! WANG     *
      * APA02846!04.06.19!2.29! NEU: K-VTAB FÜR PLANET-IM ! STORCH   *
      * APA02879!04.11.19!2.30! NEU: K-VTAB FÜR DVAGHYP   ! KRANZ    *
      * APA02908!20.02.20!2.31! NEU: K-VTAB FÜR AMV-ERW.  ! STORCH   *
      * DSY03267!19.01.21!2.32! NEU: K-VTAB FÜR AMV-ERW.  ! STORCH   *
      * DSY03552!08.06.21!    ! EINF. SONST.GES.AT HAFTPF.! KRANZ    *
      * DSY04340!11.01.22!    ! NEU: K-VTAB SONDERHAFT-KZ ! STORCH   *
      * DSY04569!12.05.22!    ! NEU: K-VTAB-VERTRAGSTEIL  ! SCHEIDEL *
      * DSY04791!05.08.22!    ! NEU: K-VTAB FÜR GEIGER-UST! WANG     *
      * DSY05194!03.02.23! NEU: K-VTAB FÜR INFINA         ! BUCH     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    KONSTANTEN FÜR MÖGLICHEN ORT, AN DEM VERARBEITUNGSSTATUS
      *    AUFTRITT
       01  K-VTAB-UNBESTIMMT        PIC X(030)
                                VALUE 'UNBESTIMMT                    '.
       01  K-VTAB-PEWP-BETR         PIC X(030)
                                VALUE 'PEWP-BETR                     '.
       01  K-VTAB-PEWP-EINH         PIC X(030)
                                VALUE 'PEWP-EINH                     '.
       01  K-VTAB-PGEB-GES          PIC X(030)
                                VALUE 'PGEB-GES                      '.
       01  K-VTAB-PEWA-AN-AUFN-DAT  PIC X(030)
                                VALUE 'PEWA-AN-AUFN-DAT              '.
       01  K-VTAB-PEWA-BEGINN       PIC X(030)
                                VALUE 'PEWA-BEGINN                   '.
       01  K-VTAB-PEWA-STRUK-DAT    PIC X(030)
                                VALUE 'PEWA-STRUK-DAT                '.
       01  K-VTAB-KNR-ZAHLWEISE     PIC X(030)
                                VALUE 'KNR-ZAHLWEISE                 '.
       01  K-VTAB-PEWA-AVERM        PIC X(030)
                                VALUE 'PEWA-AVERM                    '.
       01  K-VTAB-PROD-PROD-SCHL    PIC X(030)
                                VALUE 'PROD-PROD-SCHL                '.
       01  K-VTAB-PEWP-BEITRAG      PIC X(030)
                                VALUE 'PEWP-BEITRAG                  '.
       01  K-VTAB-PEWP-LAUFZEIT     PIC X(030)
                                VALUE 'PEWP-LAUFZEIT                 '.
       01  K-VTAB-MA-ST-NR          PIC X(030)
                                VALUE 'MA-ST-NR                      '.
       01  K-VTAB-PRZ               PIC X(030)
                                VALUE 'PRZ                           '.
       01  K-VTAB-EMP-STUFE         PIC X(030)
                                VALUE 'EMP-STUFE                     '.
       01  K-VTAB-DIFF-PROV-STUFE   PIC X(030)
                                VALUE 'DIFF-PROV-STUFE               '.
       01  K-VTAB-KNR-EMP-TYP       PIC X(030)
                                VALUE 'KNR-EMP-TYP                   '.
       01  K-VTAB-PEWA-KUNDE-NAME   PIC X(030)
                                VALUE 'PEWA-KUNDE-NAME               '.
       01  K-VTAB-PEWA-KUNDE-VNAME  PIC X(030)
                                VALUE 'PEWA-KUNDE-VNAME              '.
       01  K-VTAB-PEWA-AN-ARB-DAT   PIC X(030)
                                VALUE 'PEWA-AN-ARB-DAT               '.
       01  K-VTAB-PEWA-VOR-VNR-KZ   PIC X(030)
                                VALUE 'PEWA-VOR-VNR-KZ               '.
       01  K-VTAB-KNR-PEWA-ZUSTAND  PIC X(030)
                                VALUE 'KNR-PEWA-ZUSTAND              '.
       01  K-VTAB-KNR-STORNO-KZ     PIC X(030)
                                VALUE 'KNR-STORNO-KZ                 '.
       01  K-VTAB-KNR-RUECKMELD-STAT
                                    PIC X(030)
                                VALUE 'KNR-RUECKMELD-STAT            '.
       01  K-VTAB-VNR               PIC X(030)
                                VALUE 'VNR                           '.
       01  K-VTAB-VVNR1             PIC X(030)
                                VALUE 'VVNR1                         '.
       01  K-VTAB-VVNR2             PIC X(030)
                                VALUE 'VVNR2                         '.
       01  K-VTAB-VVNR3             PIC X(030)
                                VALUE 'VVNR3                         '.
       01  K-VTAB-VVNR4             PIC X(30)
                                VALUE 'VVNR4                         '.
       01  K-VTAB-VVNR5             PIC X(30)
                                VALUE 'VVNR5                         '.
       01  K-VTAB-VVNR6             PIC X(30)
                                VALUE 'VVNR6                         '.
       01  K-VTAB-VVGES1            PIC X(30)
                                VALUE 'VVRGES1                       '.
       01  K-VTAB-VVGES2            PIC X(30)
                                VALUE 'VVRGES2                       '.
       01  K-VTAB-VVGES3            PIC X(30)
                                VALUE 'VVRGES3                       '.
       01  K-VTAB-VVGES4            PIC X(30)
                                VALUE 'VVRGES4                       '.
       01  K-VTAB-VVGES5            PIC X(30)
                                VALUE 'VVRGES5                       '.
       01  K-VTAB-VVGES6            PIC X(30)
                                VALUE 'VVRGES6                       '.
       01  K-VTAB-VERTRAG-BEGINN    PIC X(030)
                                VALUE 'VERTRAG-BEGINN                '.
       01  K-VTAB-VERTRAG-ABLAUF    PIC X(030)
                                VALUE 'VERTRAG-ABLAUF                '.
       01  K-VTAB-FAELLIG-VON       PIC X(030)
                                VALUE 'FAELLIG-VON                   '.
       01  K-VTAB-FAELLIG-BIS       PIC X(030)
                                VALUE 'FAELLIG-BIS                   '.
       01  K-VTAB-AVERM             PIC X(030)
                                VALUE 'AVERM                         '.
       01  K-VTAB-KUNDE-NAME        PIC X(030)
                                VALUE 'KUNDE-NAME                    '.
       01  K-VTAB-KUNDE-VNAME       PIC X(030)
                                VALUE 'KUNDE-VNAME                   '.
       01  K-VTAB-ABR-DAT-PG        PIC X(030)
                                VALUE 'ABR-DAT-PG                    '.
       01  K-VTAB-EINL-DAT-PG       PIC X(030)
                                VALUE 'EINL-DAT-PG                   '.
       01  K-VTAB-PIN               PIC X(030)
                                VALUE 'PIN                           '.
       01  K-VTAB-ZAHLWEISE         PIC X(030)
                                VALUE 'ZAHLWEISE                     '.
       01  K-VTAB-WAEHRUNG          PIC X(030)
                                VALUE 'WAEHRUNG                      '.
       01  K-VTAB-PGV-PG            PIC X(030)
                                VALUE 'PGV-PG                        '.
       01  K-VTAB-PPFL-BEITRAG      PIC X(030)
                                VALUE 'PPFL-BEITRAG                  '.
       01  K-VTAB-PROV-SATZ         PIC X(030)
                                VALUE 'PROV-SATZ                     '.
       01  K-VTAB-PROV-BETRAG       PIC X(030)
                                VALUE 'PROV-BETRAG                   '.
       01  K-VTAB-JAHRES-NT-PRAEMIE PIC X(030)
                                VALUE 'JAHRES-NT-PRAEMIE             '.
       01  K-VTAB-TARIF             PIC X(030)
                                VALUE 'TARIF                         '.
       01  K-VTAB-PGV               PIC X(030)
                                VALUE 'PGV                           '.
       01  K-VTAB-VBNR-PG           PIC X(030)
                                VALUE 'VBNR-PG                       '.
       01  K-VTAB-VERTR-BEGINN      PIC X(030)
                                VALUE 'VERTR-BEGINN                  '.
       01  K-VTAB-GSMT-PROV         PIC X(030)
                                VALUE 'GSMT-PROV                     '.
       01  K-VTAB-GSMT-PROV-DAT     PIC X(030)
                                VALUE 'GSMT-PROV-DAT                 '.
       01  K-VTAB-PRINTE            PIC X(030)
                                VALUE 'PRINTE                        '.
       01  K-VTAB-PEST-ID           PIC X(030)
                                VALUE 'PEST-ID                       '.
       01  K-VTAB-AKZ               PIC X(030)
                                VALUE 'AKZ                           '.
       01  K-VTAB-PG-PM-DATEN       PIC X(030)
                                VALUE 'PG-PM-DATEN                   '.
       01  K-VTAB-SUMME             PIC X(030)
                                VALUE 'SUMME                         '.
       01  K-VTAB-SPARBETRAG        PIC X(030)
                                VALUE 'SPARBETRAG                    '.
       01  K-VTAB-VERFUEGART        PIC X(030)
                                VALUE 'VERFUEGART                    '.
       01  K-VTAB-VERM-GES          PIC X(030)
                                VALUE 'VERM-GES                      '.
       01  K-VTAB-UMW-NR            PIC X(030)
                                VALUE 'UMW-NR                        '.
       01  K-VTAB-UNR               PIC X(030)
                                VALUE 'UNR                           '.
       01  K-VTAB-SCHICHT-NR        PIC X(030)
                                VALUE 'SCHICHT-NR                    '.
       01  K-VTAB-INKASSO-DAT       PIC X(030)
                                VALUE 'INKASSO-DAT                   '.
       01  K-VTAB-INKASSOART        PIC X(030)
                                VALUE 'INKASSOART                    '.
       01  K-VTAB-SAMMELNR          PIC X(030)
                                VALUE 'SAMMELNR                      '.
       01  K-VTAB-VVG-KZ            PIC X(030)
                                VALUE 'VVG-KZ                        '.
       01  K-VTAB-DEPOT-KZ          PIC X(030)
                                VALUE 'DEPOT-KZ                      '.
       01  K-VTAB-BEW-DATUM         PIC X(030)
                                VALUE 'BEW-DATUM                     '.
       01  K-VTAB-UR-POL-DAT        PIC X(030)
                                VALUE 'UR-POL-DAT                    '.
       01  K-VTAB-POL-DAT           PIC X(030)
                                VALUE 'POL-DAT                       '.
       01  K-VTAB-BESVV             PIC X(030)
                                VALUE 'BESVV                         '.
       01  K-VTAB-DYN-KZ-1          PIC X(030)
                                VALUE 'DYN-KZ-1                      '.
       01  K-VTAB-DYN-KZ-2          PIC X(030)
                                VALUE 'DYN-KZ-2                      '.
       01  K-VTAB-BEW-HRKFT         PIC X(030)
                                VALUE 'BEW-HRKFT                     '.
       01  K-VTAB-PRO-RATA-Z        PIC X(030)
                                VALUE 'PRO-RATA-Z                    '.
       01  K-VTAB-PRO-RATA-N        PIC X(030)
                                VALUE 'PRO-RATA-N                    '.
       01  K-VTAB-BEW-SUMME         PIC X(030)
                                VALUE 'BEW-SUMME                     '.
       01  K-VTAB-RBEW-SUMME        PIC X(030)
                                VALUE 'RBEW-SUMME                    '.
       01  K-VTAB-BZ-DAUER          PIC X(030)
                                VALUE 'DAUER                         '.
       01  K-VTAB-BEITRAG-JAHR      PIC X(030)
                                VALUE 'BEITRAG-JAHR                  '.
       01  K-VTAB-PRODART           PIC X(030)
                                VALUE 'PRODART                       '.
       01  K-VTAB-NEUSTORNO-KZ      PIC X(030)
                                VALUE 'NEUSTORNO-KZ                  '.
       01  K-VTAB-TARIF-KZ          PIC X(030)
                                VALUE 'TARIF-KZ                      '.
       01  K-VTAB-GESAMT-DAUER      PIC X(030)
                                VALUE 'GESAMT-DAUER                  '.
       01  K-VTAB-HAFTZ-VON         PIC X(030)
                                VALUE 'HAFTZ-VON                     '.
       01  K-VTAB-HAFTZ-BIS         PIC X(030)
                                VALUE 'HAFTZ-BIS                     '.
       01  K-VTAB-RWERT-BEITRAG     PIC X(030)
                                VALUE 'RWERT-BEITRAG                 '.
       01  K-VTAB-VERS-SUMME        PIC X(030)
                                VALUE 'VERS-SUMME                    '.
       01  K-VTAB-BUZ-TARIF         PIC X(030)
                                VALUE 'BUZ-TARIF                     '.
       01  K-VTAB-VP-NAME           PIC X(030)
                                VALUE 'VP-NAME                       '.
       01  K-VTAB-VP-VORNAME        PIC X(030)
                                VALUE 'VP-VORNAME                    '.
       01  K-VTAB-PRO-RATA-SUMME    PIC X(030)
                                VALUE 'PRO-RATA-SUMME                '.
       01  K-VTAB-RATENBEITRAG      PIC X(030)
                                VALUE 'RATENBEITRAG                  '.
       01  K-VTAB-TARIF-GRUPPE      PIC X(030)
                                VALUE 'TARIFGRUPPE                   '.
       01  K-VTAB-FEIV-KZ           PIC X(030)
                                VALUE 'FEIV-KZ                       '.
       01  K-VTAB-RABATTSTUFE       PIC X(030)
                                VALUE 'RABATTSTUFE                   '.
       01  K-VTAB-PROV-SCHL         PIC X(030)
                                VALUE 'PROVISIONSSCHLÜSSEL           '.
       01  K-VTAB-BEW-SCHL          PIC X(030)
                                VALUE 'BEWEGUNGSSCHLÜSSEL            '.
       01  K-VTAB-UMST-SATZ         PIC X(030)
                                VALUE 'UMSATZSTEUERSATZ              '.
       01  K-VTAB-GESAMTBETRAG      PIC X(030)
                                VALUE 'GESAMTBETRAG                  '.
       01  K-VTAB-UMST-BETRAG       PIC X(030)
                                VALUE 'UMSATZSTEUERBETRAG            '.
       01  K-VTAB-PG-RECH-NR        PIC X(030)
                                VALUE 'RECHNUNGSNUMMER               '.
       01  K-VTAB-HIST-UNR          PIC X(030)
                                VALUE 'HISTORISCHE UNTERNUMMER       '.
       01  K-VTAB-FLEXN-B           PIC X(030)
                                VALUE 'FLEX-N BERECHTIGT             '.
       01  K-VTAB-FLEXN-R           PIC X(030)
                                VALUE 'FLEX-N RELEVANT               '.
       01  K-VTAB-KZ-VORSCH         PIC X(030)
                                VALUE 'KZ-VORSCHÜSSIGE PROVISION     '.
       01  K-VTAB-TL-VERBRAUCH      PIC X(030)
                                VALUE 'TELESON VERBRAUCH             '.
       01  K-VTAB-TL-ZAEHLERNR      PIC X(030)
                                VALUE 'TELESON ZÄHLERNUMMER          '.
       01  K-VTAB-TL-STORNOGRUND    PIC X(030)
                                VALUE 'TELESON STORNOGRUND           '.
       01  K-VTAB-TL-PRODPARTNER    PIC X(030)
                                VALUE 'TELESON PRODUKTPARTNER        '.
       01  K-VTAB-TL-TARIF-BEZ      PIC X(030)
                                VALUE 'TELESON TARIFBEZEICHNUNG      '.
       01  K-VTAB-T-NUMMER          PIC X(030)
                                VALUE 'T-NUMMER                      '.
       01  K-VTAB-KENNUNG           PIC X(030)
                                VALUE 'KENNUNG                       '.
       01  K-VTAB-GEVO              PIC X(030)
                                VALUE 'GEVO                          '.
       01  K-VTAB-EINALTER1         PIC X(030)
                                VALUE 'EINALTER1                     '.
       01  K-VTAB-RABAT-SATZ        PIC X(030)
                                VALUE 'RABAT-SATZ                    '.
       01  K-VTAB-SONDERHAFT-KZ     PIC X(030)
                                VALUE 'SONDERHAFT-KZ                 '.       
       01  K-VTAB-VERTRAGSTEIL      PIC X(030)
                                VALUE 'VERTRAGSTEIL                  '.       
      *
      *    KONSTANTEN FÜR MÖGLICHEN BEREICH, AN DEM VERARBEITUNGSSTATUS
      *    AUFTRITT
       01  K-VTAB-ANTRAG            PIC X(010) VALUE 'ANTRAG    '.
       01  K-VTAB-PRODUKT           PIC X(010) VALUE 'PRODUKT   '.
       01  K-VTAB-STRUKTUR          PIC X(010) VALUE 'STRUKTUR  '.
       01  K-VTAB-ALLG              PIC X(010) VALUE 'ALLGEMEIN '.
       01  K-VTAB-DTA-AMV           PIC X(010) VALUE 'DTA-AMV   '.
       01  K-VTAB-VERTRAG           PIC X(010) VALUE 'VERTRAG   '.
       01  K-VTAB-PRODUKTTEIL       PIC X(010) VALUE 'PRODUKTEIL'.
       01  K-VTAB-MELDUNG           PIC X(010) VALUE 'MELDUNG   '.
       01  K-VTAB-BUCHUNG           PIC X(010) VALUE 'BUCHUNG   '.
       01  K-VTAB-DTA-BADENIA       PIC X(010) VALUE 'DTABADENIA'.
       01  K-VTAB-DTA-III           PIC X(010) VALUE 'DTAIII    '.
       01  K-VTAB-DTA-DIT           PIC X(010) VALUE 'DTA-DIT   '.
       01  K-VTAB-DTA-HYPO          PIC X(010) VALUE 'DTA-HYPO  '.
       01  K-VTAB-DTA-BFG           PIC X(010) VALUE 'DTA-BFG   '.
       01  K-VTAB-DTA-COMMERZBANK   PIC X(010) VALUE 'DTA-COBA  '.
       01  K-VTAB-DTA-CENTRAL       PIC X(010) VALUE 'DTA-CENTRA'.
       01  K-VTAB-DTA-DGI           PIC X(010) VALUE 'DTA-DGI   '.
       01  K-VTAB-DTA-AML-DARL      PIC X(010) VALUE 'DTA-AML-DL'.
       01  K-VTAB-DTA-BFG-BANK      PIC X(010) VALUE 'DTA-BFG-BK'.
       01  K-VTAB-DTA-BAWAG         PIC X(010) VALUE 'DTA-BAWAG '.
       01  K-VTAB-DTA-DREBA         PIC X(010) VALUE 'DTA-DREBA '.
       01  K-VTAB-DTA-ADVOCARD      PIC X(010) VALUE 'DTA-ADVOCA'.
       01  K-VTAB-DTA-DREBA-CH      PIC X(010) VALUE 'DTA-DREBCH'.
       01  K-VTAB-DTA-DMB           PIC X(010) VALUE 'DTA-DMB   '.
       01  K-VTAB-DTA-DIT-OEST      PIC X(010) VALUE 'DTA-DIT-OE'.
       01  K-VTAB-DTA-BANK-AUSTRIA  PIC X(010) VALUE 'DTA-B-AUST'.
       01  K-VTAB-DTA-BANK-AUSTR-I  PIC X(010) VALUE 'DTA-B-AUSI'.
       01  K-VTAB-DTA-DWS           PIC X(010) VALUE 'DTA-DWS   '.
       01  K-VTAB-DTA-DB24          PIC X(010) VALUE 'DTA-DB24  '.
       01  K-VTAB-DTA-WIENER-SACH   PIC X(010) VALUE 'DTA-WIEN-S'.
       01  K-VTAB-DTA-GENER-SACH    PIC X(010) VALUE 'DTA-GENE-S'.
       01  K-VTAB-DTA-WIENER-LEBEN  PIC X(010) VALUE 'DTA-WIEN-L'.
       01  K-VTAB-DTA-GENER-LEBEN   PIC X(010) VALUE 'DTA-GENE-L'.
       01  K-VTAB-DTA-DWS-AUSTRIA   PIC X(010) VALUE 'DTA-DWS-A '.
       01  K-VTAB-DTA-S-BAUSPAR-K   PIC X(010) VALUE 'DTA-S-BSPK'.
       01  K-VTAB-DTA-GENERALI-CH   PIC X(010) VALUE 'DTA-GENE-C'.
       01  K-VTAB-DTA-SONST-GES-D   PIC X(010) VALUE 'DTA-SONSTD'.
       01  K-VTAB-DTA-DSL-BANK      PIC X(010) VALUE 'DTA-DSL'.
       01  K-VTAB-DTA-GENERALI-B    PIC X(010) VALUE 'DTA-GENE-B'.
       01  K-VTAB-DTA-GENERALI-I    PIC X(010) VALUE 'DTA-GENE-I'.
       01  K-VTAB-DTA-SONST-GES-A   PIC X(010) VALUE 'DTA-SONSTA'.
       01  K-VTAB-DTA-AM-LEBEN      PIC X(010) VALUE 'DTA-AML   '.
       01  K-VTAB-DTA-PENKA         PIC X(010) VALUE 'DTA-PENKA '.
       01  K-VTAB-DTA-DVM           PIC X(010) VALUE 'DTA-DVM   '.
       01  K-VTAB-DTA-CKV-VP        PIC X(010) VALUE 'DTA-CKV-VP'.
       01  K-VTAB-DTA-TELESON       PIC X(010) VALUE 'DTA-TESN'  .
       01  K-VTAB-DTA-FINGERHAUS    PIC X(010) VALUE 'DTA-F-HAUS'.
       01  K-VTAB-DTA-DEVOLO        PIC X(010) VALUE 'DTA-DEVOLO'.
       01  K-VTAB-DTA-LOHNWERK      PIC X(010) VALUE 'DTA-L-WERK'.
       01  K-VTAB-DTA-CADOOZ        PIC X(010) VALUE 'DTA-CADOOZ'.
       01  K-VTAB-DTA-CAPITAL       PIC X(010) VALUE 'DTA-CPITAL'.
       01  K-VTAB-DTA-GEIGER        PIC X(010) VALUE 'DTA-GEIGER'.
       01  K-VTAB-DTA-ALAG-GEV      PIC X(010) VALUE 'DTA-GEV   '.
       01  K-VTAB-DTA-ALAG-LEBEN    PIC X(010) VALUE 'DTA-GEL   '.
       01  K-VTAB-DTA-ALAG-INVEST   PIC X(010) VALUE 'DTA-GEI   '.
       01  K-VTAB-DTA-PLANET-IMMO   PIC X(010) VALUE 'DTA-PLANIM'.
       01  K-VTAB-DTA-DVAGHYP       PIC X(010) VALUE 'DTA-DVAGHY'.
       01  K-VTAB-DTA-SO-GES-AT-HP  PIC X(010) VALUE 'DTA-SGATHP'. 
050822 01  K-VTAB-DTA-GEIGER-UST    PIC X(010) VALUE 'DTA-GEIUST'.
       01  K-VTAB-DTA-INFINA        PIC X(010) VALUE 'DTA-INFINA'.              
      *


