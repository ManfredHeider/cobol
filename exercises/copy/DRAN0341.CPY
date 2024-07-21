      *****************************************************************
      *    COPY-MEMBER: DRAN0341                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRAN0341                    *
      *    --------------------------------------------------------   *
      *****************************************************************
      * LOGBUCH:                                                      *
      * AUFTRAG    ! DATUM  ! AENDERUNG                    ! NAME     *
      * --------------------------------------------------------------*
      * ANTRAG-1660!18.07.22! NEUES ATTRIBUT PGEB-GES      ! HEIDER   *
      *            !        !                              !          *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRAN0341
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRAN0341.

180722       10 PI-DRAN0341-GEIG-PGEB-GES       PIC S9(4) COMP.
             10 PI-DRAN0341-GEIG-VORSCHLAG-NR   PIC  X(0009).

      *
       01  PO-DRAN0341.
           05  PO-DRAN0341-RC                   PIC  X(003).
               88 PO-DRAN0341-RC-OK             VALUE 'OK '.
               88 PO-DRAN0341-RC-NOT-FOUND      VALUE 'NFD'.
      *
           05  PO-DRAN0341-DATEN.

             10 PO-DRAN0341-GEIG-ANREDE         PIC  X(0005).
             10 PO-DRAN0341-GEIG-AVERM          PIC S9(9) COMP.
             10 PO-DRAN0341-GEIG-AVERM-PZ       PIC  X(0001).
             10 PO-DRAN0341-GEIG-BANK-NAME      PIC  X(0050).
             10 PO-DRAN0341-GEIG-EMAIL          PIC  X(0050).
             10 PO-DRAN0341-GEIG-FIRMA          PIC  X(0050).
             10 PO-DRAN0341-GEIG-GEBURT-DATUM   PIC  X(010).
             10 PO-DRAN0341-GEIG-IBAN           PIC  X(0034).
             10 PO-DRAN0341-GEIG-KENNUNG-INS    PIC  X(0008).
             10 PO-DRAN0341-GEIG-KENNUNG-UP     PIC  X(0008).
             10 PO-DRAN0341-GEIG-KUNDE-NR       PIC S9(18)V COMP-3.
             10 PO-DRAN0341-GEIG-LKZ            PIC  X(0003).
             10 PO-DRAN0341-GEIG-NAME           PIC  X(0026).
             10 PO-DRAN0341-GEIG-NAME-K-INH     PIC  X(0026).
             10 PO-DRAN0341-GEIG-OKI-ZEITPUNKT  PIC  X(0026).
             10 PO-DRAN0341-GEIG-ORT            PIC  X(0030).
             10 PO-DRAN0341-GEIG-PIN            PIC S9(9) COMP.
             10 PO-DRAN0341-GEIG-PLZ            PIC  X(0006).
             10 PO-DRAN0341-GEIG-STEUERNUMMER   PIC  X(0013).
             10 PO-DRAN0341-GEIG-STRASSE        PIC  X(0045).
             10 PO-DRAN0341-GEIG-TELEFON        PIC  X(0030).
             10 PO-DRAN0341-GEIG-TITEL          PIC  X(0020).
             10 PO-DRAN0341-GEIG-VERSION        PIC  X(0004).
             10 PO-DRAN0341-GEIG-VM             PIC S9(9) COMP.
             10 PO-DRAN0341-GEIG-VM-PZ          PIC  X(0001).
             10 PO-DRAN0341-GEIG-VNAME-K-INH    PIC  X(0026).
             10 PO-DRAN0341-GEIG-VORNAME        PIC  X(0026).
             10 PO-DRAN0341-GEIG-ZEIT-INSERT    PIC  X(0026).
             10 PO-DRAN0341-GEIG-ZEIT-UPDATE    PIC  X(0026).
      *
      ******* ENDE COPY-MEMBER ***************************************
