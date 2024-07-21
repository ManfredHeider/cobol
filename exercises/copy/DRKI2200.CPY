      *****************************************************************
      *    COPY-MEMBER: DRKI2200                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2200                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         13.03.2023 / 12:44:24 / T13EX78
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
      *    FÜR DIE FUNKTION DRKI2200
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2200.
           05  PI-DRKI2200-KEY.

ANF   *        %M_PI_KEY%
             10 PI-DRKI2200-VIFVERTRAGSID   PIC  X(0013).
END   *

           05  PI-DRKI2200-NI.

ANF   *        %M_PI_DATEN_NI%
             15 PI-DRKI2200-NI-VIFAKTERGEBNIS   PIC S9(4) COMP.
             15 PI-DRKI2200-NI-VIFBESTMANAGKZ   PIC S9(4) COMP.
             15 PI-DRKI2200-N-VFBNDLKZN-KNR     PIC S9(4) COMP.
             15 PI-DRKI2200-N-VFDTMGTBWG        PIC S9(4) COMP.
             15 PI-DRKI2200-NI-VIFERSTELLDATUM  PIC S9(4) COMP.
             15 PI-DRKI2200-N-VFFLLGKDTM        PIC S9(4) COMP.
             15 PI-DRKI2200-NI-VIFFLEXNVGVKZ    PIC S9(4) COMP.
             15 PI-DRKI2200-NI-VIFFLEXNVGVNL    PIC S9(4) COMP.
             15 PI-DRKI2200-NI-VIFFLEXNVSPKZ    PIC S9(4) COMP.
             15 PI-DRKI2200-NI-VIFFLEXNVSPNL    PIC S9(4) COMP.
             15 PI-DRKI2200-NI-VIFKUNDENIDSMH   PIC S9(4) COMP.
             15 PI-DRKI2200-NI-VIFMAHNDATUM     PIC S9(4) COMP.
             15 PI-DRKI2200-N-VFPRKMBBZ-KNR     PIC S9(4) COMP.
             15 PI-DRKI2200-NI-VIFSYNCDATUM     PIC S9(4) COMP.
END   *

           05  PI-DRKI2200-DATEN.

ANF   *        %M_PI_DATEN%
             10 PI-DRKI2200-VIFAENDERUNGSGRD   PIC  X(0050).
             10 PI-DRKI2200-VIFAFBANTEILPROZ   PIC S9(2)V9(3) COMP-3.
             10 PI-DRKI2200-VIFAKTERGEBNIS   PIC  X(0003).
             10 PI-DRKI2200-VIFANTRAGSNUMMER   PIC  X(0018).
             10 PI-DRKI2200-VIFBESTMANAGKZ   PIC  X(0001).
             10 PI-DRKI2200-VIFBUENDELKZN-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI2200-VIFDATUMAGTBEWEG   PIC  X(010).
             10 PI-DRKI2200-VIFDATUMBEWEGUNG   PIC  X(0061).
             10 PI-DRKI2200-VIFEINLBEITRBRT   PIC S9(8)V9(2) COMP-3.
             10 PI-DRKI2200-VIFEINLBEITRNET   PIC S9(8)V9(2) COMP-3.
             10 PI-DRKI2200-VIFERSTELLDATUM   PIC  X(010).
             10 PI-DRKI2200-VIFERSTELLZEIT   PIC S9(9) COMP.
             10 PI-DRKI2200-VIFFAELLIGKDATUM   PIC  X(010).
             10 PI-DRKI2200-VIFFBBETRAGBRT   PIC S9(8)V9(2) COMP-3.
             10 PI-DRKI2200-VIFFBSUMME   PIC S9(9) COMP.
             10 PI-DRKI2200-VIFFLEXNVGVKZ   PIC  X(0001).
             10 PI-DRKI2200-VIFFLEXNVGVNL   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI2200-VIFFLEXNVSPKZ   PIC  X(0001).
             10 PI-DRKI2200-VIFFLEXNVSPNL   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI2200-VIFHERKUNFT   PIC  X(0003).
             10 PI-DRKI2200-VIFJGBBEITRAG   PIC S9(10)V9(2) COMP-3.
             10 PI-DRKI2200-VIFJGNBEITRAG   PIC S9(8)V9(2) COMP-3.
             10 PI-DRKI2200-VIFKUNDENBONUS   PIC  X(0026).
             10 PI-DRKI2200-VIFKUNDENIDSMH   PIC  X(0010).
             10 PI-DRKI2200-VIFKURZTEXTBSA   PIC  X(0006).
             10 PI-DRKI2200-VIFLANGTEXTBSA   PIC  X(0050).
             10 PI-DRKI2200-VIFLZRABTTPROZ   PIC S9(4) COMP.
             10 PI-DRKI2200-VIFMAHNDATUM   PIC  X(010).
             10 PI-DRKI2200-VIFMAHNSTUFE   PIC  X(0025).
             10 PI-DRKI2200-VIFOSTRABATT   PIC S9(4) COMP.
             10 PI-DRKI2200-VIFPRKOMBBEZ-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI2200-VIFPRKOMBBEZ-KTY   PIC S9(4) COMP.
             10 PI-DRKI2200-VIFQUITTUNGSNR   PIC  X(0015).
             10 PI-DRKI2200-VIFRATENZUSCHLAG   PIC S9(4) COMP.
             10 PI-DRKI2200-VIFRUECKSTBETRAG   PIC S9(7)V9(2) COMP-3.
             10 PI-DRKI2200-VIFSYNCDATUM   PIC  X(010).
             10 PI-DRKI2200-VIFTARIFPRODUKT   PIC  X(0050).
             10 PI-DRKI2200-VIFVDNAME   PIC  X(0020).
             10 PI-DRKI2200-VIFVDNR   PIC S9(9) COMP.
             10 PI-DRKI2200-VIFVERMITTLER1NR   PIC  X(0010).
             10 PI-DRKI2200-VIFVERMITTLER2NR   PIC  X(0010).
             10 PI-DRKI2200-VIFVERMIT1MERKM   PIC  X(0020).
             10 PI-DRKI2200-VIFVERMIT2MERKM   PIC  X(0020).
             10 PI-DRKI2200-VIFVERSGESELL   PIC  X(0050).
             10 PI-DRKI2200-VIFVERSICHRISIKO   PIC  X(0023).
             10 PI-DRKI2200-VIFVERSSUMME   PIC S9(10)V COMP-3.
             10 PI-DRKI2200-VIFVERTRAGARTVTG   PIC S9(9) COMP.
             10 PI-DRKI2200-VIFVERTRAGSINFO   PIC  X(0080).
             10 PI-DRKI2200-VIFVERTSPAR-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI2200-VIFVORVERTRAGSNR   PIC  X(0017).
             10 PI-DRKI2200-VIFVSPKZ   PIC  X(0001).
             10 PI-DRKI2200-VIFZEICHNUNG   PIC  X(0025).
END   *

      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI2200-NI1.
           05 PI-DRKI2200-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                      OCCURS 14.
       01  PI-DRKI2200-NI7.
           05 PI-DRKI2200-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                      OCCURS 14.

      *
       01  PO-DRKI2200.
           05  PO-DRKI2200-RC                     PIC  X(003).
               88 PO-DRKI2200-RC-OK                  VALUE 'OK '.
               88 PO-DRKI2200-RC-DUPLICATE-VALUE VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************
