      *****************************************************************
      *    COPY-MEMBER: DRKI2201                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2201                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         13.03.2023 / 12:44:27 / T13EX78
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI2201
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2201.
           05  PI-DRKI2201-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI2201-VIFVERTRAGSID   PIC  X(0013).
END   *

           05  PI-DRKI2201-NI.

ANF   *        %M_PI_DATEN_NI%
             15 PI-DRKI2201-N-VFNDRNGSGRD       PIC S9(4) COMP.
             15 PI-DRKI2201-N-VFFBNTLPRZ        PIC S9(4) COMP.
             15 PI-DRKI2201-NI-VIFAKTERGEBNIS   PIC S9(4) COMP.
             15 PI-DRKI2201-N-VFNTRGSNMMR       PIC S9(4) COMP.
             15 PI-DRKI2201-NI-VIFBESTMANAGKZ   PIC S9(4) COMP.
             15 PI-DRKI2201-N-VFBNDLKZN-KNR     PIC S9(4) COMP.
             15 PI-DRKI2201-N-VFDTMGTBWG        PIC S9(4) COMP.
             15 PI-DRKI2201-N-VFDTMBWGNG        PIC S9(4) COMP.
             15 PI-DRKI2201-NI-VIFEINLBEITRBRT  PIC S9(4) COMP.
             15 PI-DRKI2201-NI-VIFEINLBEITRNET  PIC S9(4) COMP.
             15 PI-DRKI2201-NI-VIFERSTELLDATUM  PIC S9(4) COMP.
             15 PI-DRKI2201-NI-VIFERSTELLZEIT   PIC S9(4) COMP.
             15 PI-DRKI2201-N-VFFLLGKDTM        PIC S9(4) COMP.
             15 PI-DRKI2201-NI-VIFFBBETRAGBRT   PIC S9(4) COMP.
             15 PI-DRKI2201-NI-VIFFBSUMME       PIC S9(4) COMP.
             15 PI-DRKI2201-NI-VIFFLEXNVGVKZ    PIC S9(4) COMP.
             15 PI-DRKI2201-NI-VIFFLEXNVGVNL    PIC S9(4) COMP.
             15 PI-DRKI2201-NI-VIFFLEXNVSPKZ    PIC S9(4) COMP.
             15 PI-DRKI2201-NI-VIFFLEXNVSPNL    PIC S9(4) COMP.
             15 PI-DRKI2201-NI-VIFHERKUNFT      PIC S9(4) COMP.
             15 PI-DRKI2201-NI-VIFJGBBEITRAG    PIC S9(4) COMP.
             15 PI-DRKI2201-NI-VIFJGNBEITRAG    PIC S9(4) COMP.
             15 PI-DRKI2201-NI-VIFKUNDENBONUS   PIC S9(4) COMP.
             15 PI-DRKI2201-NI-VIFKUNDENIDSMH   PIC S9(4) COMP.
             15 PI-DRKI2201-NI-VIFKURZTEXTBSA   PIC S9(4) COMP.
             15 PI-DRKI2201-NI-VIFLANGTEXTBSA   PIC S9(4) COMP.
             15 PI-DRKI2201-NI-VIFLZRABTTPROZ   PIC S9(4) COMP.
             15 PI-DRKI2201-NI-VIFMAHNDATUM     PIC S9(4) COMP.
             15 PI-DRKI2201-NI-VIFMAHNSTUFE     PIC S9(4) COMP.
             15 PI-DRKI2201-NI-VIFOSTRABATT     PIC S9(4) COMP.
             15 PI-DRKI2201-N-VFPRKMBBZ-KNR     PIC S9(4) COMP.
             15 PI-DRKI2201-N-VFPRKMBBZ-KTY     PIC S9(4) COMP.
             15 PI-DRKI2201-NI-VIFQUITTUNGSNR   PIC S9(4) COMP.
             15 PI-DRKI2201-N-VFRTNZSCHLG       PIC S9(4) COMP.
             15 PI-DRKI2201-N-VFRCKSTBTRG       PIC S9(4) COMP.
             15 PI-DRKI2201-NI-VIFSYNCDATUM     PIC S9(4) COMP.
             15 PI-DRKI2201-NI-VIFTARIFPRODUKT  PIC S9(4) COMP.
             15 PI-DRKI2201-NI-VIFVDNAME        PIC S9(4) COMP.
             15 PI-DRKI2201-NI-VIFVDNR          PIC S9(4) COMP.
             15 PI-DRKI2201-N-VFVRMTTLR1NR      PIC S9(4) COMP.
             15 PI-DRKI2201-N-VFVRMTTLR2NR      PIC S9(4) COMP.
             15 PI-DRKI2201-NI-VIFVERMIT1MERKM  PIC S9(4) COMP.
             15 PI-DRKI2201-NI-VIFVERMIT2MERKM  PIC S9(4) COMP.
             15 PI-DRKI2201-NI-VIFVERSGESELL    PIC S9(4) COMP.
             15 PI-DRKI2201-N-VFVRSCHRSK        PIC S9(4) COMP.
             15 PI-DRKI2201-NI-VIFVERSSUMME     PIC S9(4) COMP.
             15 PI-DRKI2201-N-VFVRTRGRTVTG      PIC S9(4) COMP.
             15 PI-DRKI2201-NI-VIFVERTRAGSINFO  PIC S9(4) COMP.
             15 PI-DRKI2201-NI-VIFVERTSPAR-KNR  PIC S9(4) COMP.
             15 PI-DRKI2201-N-VFVRVRTRGSNR      PIC S9(4) COMP.
             15 PI-DRKI2201-NI-VIFVSPKZ         PIC S9(4) COMP.
             15 PI-DRKI2201-NI-VIFZEICHNUNG     PIC S9(4) COMP.
END   *

           05  PI-DRKI2201-DATEN.

ANF   *        %M_PI_DATEN%
             10 PI-DRKI2201-VIFAENDERUNGSGRD   PIC  X(0050).
             10 PI-DRKI2201-VIFAFBANTEILPROZ   PIC S9(2)V9(3) COMP-3.
             10 PI-DRKI2201-VIFAKTERGEBNIS   PIC  X(0003).
             10 PI-DRKI2201-VIFANTRAGSNUMMER   PIC  X(0018).
             10 PI-DRKI2201-VIFBESTMANAGKZ   PIC  X(0001).
             10 PI-DRKI2201-VIFBUENDELKZN-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI2201-VIFDATUMAGTBEWEG   PIC  X(010).
             10 PI-DRKI2201-VIFDATUMBEWEGUNG   PIC  X(0061).
             10 PI-DRKI2201-VIFEINLBEITRBRT   PIC S9(8)V9(2) COMP-3.
             10 PI-DRKI2201-VIFEINLBEITRNET   PIC S9(8)V9(2) COMP-3.
             10 PI-DRKI2201-VIFERSTELLDATUM   PIC  X(010).
             10 PI-DRKI2201-VIFERSTELLZEIT   PIC S9(9) COMP.
             10 PI-DRKI2201-VIFFAELLIGKDATUM   PIC  X(010).
             10 PI-DRKI2201-VIFFBBETRAGBRT   PIC S9(8)V9(2) COMP-3.
             10 PI-DRKI2201-VIFFBSUMME   PIC S9(9) COMP.
             10 PI-DRKI2201-VIFFLEXNVGVKZ   PIC  X(0001).
             10 PI-DRKI2201-VIFFLEXNVGVNL   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI2201-VIFFLEXNVSPKZ   PIC  X(0001).
             10 PI-DRKI2201-VIFFLEXNVSPNL   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI2201-VIFHERKUNFT   PIC  X(0003).
             10 PI-DRKI2201-VIFJGBBEITRAG   PIC S9(10)V9(2) COMP-3.
             10 PI-DRKI2201-VIFJGNBEITRAG   PIC S9(8)V9(2) COMP-3.
             10 PI-DRKI2201-VIFKUNDENBONUS   PIC  X(0026).
             10 PI-DRKI2201-VIFKUNDENIDSMH   PIC  X(0010).
             10 PI-DRKI2201-VIFKURZTEXTBSA   PIC  X(0006).
             10 PI-DRKI2201-VIFLANGTEXTBSA   PIC  X(0050).
             10 PI-DRKI2201-VIFLZRABTTPROZ   PIC S9(4) COMP.
             10 PI-DRKI2201-VIFMAHNDATUM   PIC  X(010).
             10 PI-DRKI2201-VIFMAHNSTUFE   PIC  X(0025).
             10 PI-DRKI2201-VIFOSTRABATT   PIC S9(4) COMP.
             10 PI-DRKI2201-VIFPRKOMBBEZ-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI2201-VIFPRKOMBBEZ-KTY   PIC S9(4) COMP.
             10 PI-DRKI2201-VIFQUITTUNGSNR   PIC  X(0015).
             10 PI-DRKI2201-VIFRATENZUSCHLAG   PIC S9(4) COMP.
             10 PI-DRKI2201-VIFRUECKSTBETRAG   PIC S9(7)V9(2) COMP-3.
             10 PI-DRKI2201-VIFSYNCDATUM   PIC  X(010).
             10 PI-DRKI2201-VIFTARIFPRODUKT   PIC  X(0050).
             10 PI-DRKI2201-VIFVDNAME   PIC  X(0020).
             10 PI-DRKI2201-VIFVDNR   PIC S9(9) COMP.
             10 PI-DRKI2201-VIFVERMITTLER1NR   PIC  X(0010).
             10 PI-DRKI2201-VIFVERMITTLER2NR   PIC  X(0010).
             10 PI-DRKI2201-VIFVERMIT1MERKM   PIC  X(0020).
             10 PI-DRKI2201-VIFVERMIT2MERKM   PIC  X(0020).
             10 PI-DRKI2201-VIFVERSGESELL   PIC  X(0050).
             10 PI-DRKI2201-VIFVERSICHRISIKO   PIC  X(0023).
             10 PI-DRKI2201-VIFVERSSUMME   PIC S9(10)V COMP-3.
             10 PI-DRKI2201-VIFVERTRAGARTVTG   PIC S9(9) COMP.
             10 PI-DRKI2201-VIFVERTRAGSINFO   PIC  X(0080).
             10 PI-DRKI2201-VIFVERTSPAR-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI2201-VIFVORVERTRAGSNR   PIC  X(0017).
             10 PI-DRKI2201-VIFVSPKZ   PIC  X(0001).
             10 PI-DRKI2201-VIFZEICHNUNG   PIC  X(0025).
END   *

      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI2201-NI1.
           05 PI-DRKI2201-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                  OCCURS 52.
       01  PI-DRKI2201-NI7.
           05 PI-DRKI2201-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                  OCCURS 52.
      *
       01  PO-DRKI2201.
           05  PO-DRKI2201-RC                     PIC  X(003).
               88 PO-DRKI2201-RC-OK               VALUE 'OK '.
               88 PO-DRKI2201-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
