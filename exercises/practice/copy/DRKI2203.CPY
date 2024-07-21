      *****************************************************************
      *    COPY-MEMBER: DRKI2203                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2203                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         13.03.2023 / 12:44:31 / T13EX78
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
      *    FÜR DIE FUNKTION DRKI2203
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2203.
           05  PI-DRKI2203-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI2203-VIFVERTRAGSID   PIC  X(0013).
END   *

      *
       01  PO-DRKI2203.
           05  PO-DRKI2203-RC                     PIC  X(003).
               88 PO-DRKI2203-RC-OK               VALUE 'OK '.
               88 PO-DRKI2203-RC-NOT-FOUND        VALUE 'NFD'.
      *
           05  PO-DRKI2203-NI.
               10 NI-DRKI2203-TAB.
                  15 DRKI2203-ELE
                     PIC S9(04) COMP OCCURS 15 TIMES.
               10 NI-DRKI2203 REDEFINES NI-DRKI2203-TAB.

ANF   *        %M_PO_DATEN_NI%
             15 PO-DRKI2203-NI-VIFAKTERGEBNIS   PIC S9(4) COMP.
             15 PO-DRKI2203-NI-VIFBESTMANAGKZ   PIC S9(4) COMP.
             15 PO-DRKI2203-N-VFBNDLKZN-KNR   PIC S9(4) COMP.
             15 PO-DRKI2203-N-VFDTMGTBWG   PIC S9(4) COMP.
             15 PO-DRKI2203-NI-VIFERSTELLDATUM   PIC S9(4) COMP.
             15 PO-DRKI2203-N-VFFLLGKDTM   PIC S9(4) COMP.
             15 PO-DRKI2203-NI-VIFFLEXNVGVKZ   PIC S9(4) COMP.
             15 PO-DRKI2203-NI-VIFFLEXNVGVNL   PIC S9(4) COMP.
             15 PO-DRKI2203-NI-VIFFLEXNVSPKZ   PIC S9(4) COMP.
             15 PO-DRKI2203-NI-VIFFLEXNVSPNL   PIC S9(4) COMP.
             15 PO-DRKI2203-NI-VIFKUNDENIDSMH   PIC S9(4) COMP.
             15 PO-DRKI2203-NI-VIFMAHNDATUM   PIC S9(4) COMP.
             15 PO-DRKI2203-N-VFPRKMBBZ-KNR   PIC S9(4) COMP.
             15 PO-DRKI2203-N-VFPRKMBBZ-KTY   PIC S9(4) COMP.
             15 PO-DRKI2203-NI-VIFSYNCDATUM   PIC S9(4) COMP.
END   *

           05  PO-DRKI2203-DATEN.


ANF   *        %M_PO_DATEN%
             10 PO-DRKI2203-LETZTEAENDERUNG   PIC  X(0026).
             10 PO-DRKI2203-VIFAENDERUNGSGRD   PIC  X(0050).
             10 PO-DRKI2203-VIFAFBANTEILPROZ   PIC S9(2)V9(3) COMP-3.
             10 PO-DRKI2203-VIFAKTERGEBNIS   PIC  X(0003).
             10 PO-DRKI2203-VIFANTRAGSNUMMER   PIC  X(0018).
             10 PO-DRKI2203-VIFBESTMANAGKZ   PIC  X(0001).
             10 PO-DRKI2203-VIFBUENDELKZN-KNR   PIC S9(15)V COMP-3.
             10 PO-DRKI2203-VIFDATUMAGTBEWEG   PIC  X(010).
             10 PO-DRKI2203-VIFDATUMBEWEGUNG   PIC  X(0061).
             10 PO-DRKI2203-VIFEINLBEITRBRT   PIC S9(8)V9(2) COMP-3.
             10 PO-DRKI2203-VIFEINLBEITRNET   PIC S9(8)V9(2) COMP-3.
             10 PO-DRKI2203-VIFERSTELLDATUM   PIC  X(010).
             10 PO-DRKI2203-VIFERSTELLZEIT   PIC S9(9) COMP.
             10 PO-DRKI2203-VIFFAELLIGKDATUM   PIC  X(010).
             10 PO-DRKI2203-VIFFBBETRAGBRT   PIC S9(8)V9(2) COMP-3.
             10 PO-DRKI2203-VIFFBSUMME   PIC S9(9) COMP.
             10 PO-DRKI2203-VIFFLEXNVGVKZ   PIC  X(0001).
             10 PO-DRKI2203-VIFFLEXNVGVNL   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI2203-VIFFLEXNVSPKZ   PIC  X(0001).
             10 PO-DRKI2203-VIFFLEXNVSPNL   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI2203-VIFHERKUNFT   PIC  X(0003).
             10 PO-DRKI2203-VIFJGBBEITRAG   PIC S9(10)V9(2) COMP-3.
             10 PO-DRKI2203-VIFJGNBEITRAG   PIC S9(8)V9(2) COMP-3.
             10 PO-DRKI2203-VIFKUNDENBONUS   PIC  X(0026).
             10 PO-DRKI2203-VIFKUNDENIDSMH   PIC  X(0010).
             10 PO-DRKI2203-VIFKURZTEXTBSA   PIC  X(0006).
             10 PO-DRKI2203-VIFLANGTEXTBSA   PIC  X(0050).
             10 PO-DRKI2203-VIFLZRABTTPROZ   PIC S9(4) COMP.
             10 PO-DRKI2203-VIFMAHNDATUM   PIC  X(010).
             10 PO-DRKI2203-VIFMAHNSTUFE   PIC  X(0025).
             10 PO-DRKI2203-VIFOSTRABATT   PIC S9(4) COMP.
             10 PO-DRKI2203-VIFPRKOMBBEZ-KNR   PIC S9(15)V COMP-3.
             10 PO-DRKI2203-VIFPRKOMBBEZ-KTY   PIC S9(4) COMP.
             10 PO-DRKI2203-VIFQUITTUNGSNR   PIC  X(0015).
             10 PO-DRKI2203-VIFRATENZUSCHLAG   PIC S9(4) COMP.
             10 PO-DRKI2203-VIFRUECKSTBETRAG   PIC S9(7)V9(2) COMP-3.
             10 PO-DRKI2203-VIFSYNCDATUM   PIC  X(010).
             10 PO-DRKI2203-VIFTARIFPRODUKT   PIC  X(0050).
             10 PO-DRKI2203-VIFVDNAME   PIC  X(0020).
             10 PO-DRKI2203-VIFVDNR   PIC S9(9) COMP.
             10 PO-DRKI2203-VIFVERMITTLER1NR   PIC  X(0010).
             10 PO-DRKI2203-VIFVERMITTLER2NR   PIC  X(0010).
             10 PO-DRKI2203-VIFVERMIT1MERKM   PIC  X(0020).
             10 PO-DRKI2203-VIFVERMIT2MERKM   PIC  X(0020).
             10 PO-DRKI2203-VIFVERSGESELL   PIC  X(0050).
             10 PO-DRKI2203-VIFVERSICHRISIKO   PIC  X(0023).
             10 PO-DRKI2203-VIFVERSSUMME   PIC S9(10)V COMP-3.
             10 PO-DRKI2203-VIFVERTRAGARTVTG   PIC S9(9) COMP.
             10 PO-DRKI2203-VIFVERTRAGSINFO   PIC  X(0080).
             10 PO-DRKI2203-VIFVERTSPAR-KNR   PIC S9(15)V COMP-3.
             10 PO-DRKI2203-VIFVERTSPAR-KTY   PIC S9(4) COMP.
             10 PO-DRKI2203-VIFVORVERTRAGSNR   PIC  X(0017).
             10 PO-DRKI2203-VIFVSPKZ   PIC  X(0001).
             10 PO-DRKI2203-VIFZEICHNUNG   PIC  X(0025).
END   *


      *
      ******* ENDE COPY-MEMBER ***************************************
