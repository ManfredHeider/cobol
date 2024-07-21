      *****************************************************************
      *    COPY-MEMBER: DRAN0339                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRAN0339                    *
      *    --------------------------------------------------------   *
      *****************************************************************
      * LOGBUCH:                                                      *
      * AUFTRAG    ! DATUM  ! AENDERUNG                    ! NAME     *
      * --------------------------------------------------------------*
      * ANTRAG-939 !26.03.21! INTERNE TABELLE              ! HEIDER   *
      * ANTRAG-1246!02.11.21! PARKDEPOT (VERSION 2101) NEU ! RAHNER   *
      * ANTRAG-1660!18.07.22! NEUES ATTRIBUT PGEB-GES      ! HEIDER   *
      *            !        !                              !          *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRAN0339
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRAN0339.
           05  PI-DRAN0339-KEY.

180722       10 PI-DRAN0339-GEIG-PGEB-GES       PIC S9(4) COMP.
             10 PI-DRAN0339-GEIG-VORSCHLAG-NR   PIC  X(0009).

           05  PI-DRAN0339-DATEN.

             10 PI-DRAN0339-GEIG-ANREDE         PIC  X(0005).
             10 PI-DRAN0339-GEIG-AVERM          PIC S9(9) COMP.
             10 PI-DRAN0339-GEIG-AVERM-PZ       PIC  X(0001).
             10 PI-DRAN0339-GEIG-BANK-NAME      PIC  X(0050).
             10 PI-DRAN0339-GEIG-EMAIL          PIC  X(0050).
             10 PI-DRAN0339-GEIG-FIRMA          PIC  X(0050).
             10 PI-DRAN0339-GEIG-GEBURT-DATUM   PIC  X(010).
             10 PI-DRAN0339-GEIG-IBAN           PIC  X(0034).
             10 PI-DRAN0339-GEIG-KENNUNG-INS    PIC  X(0008).
             10 PI-DRAN0339-GEIG-KENNUNG-UP     PIC  X(0008).
             10 PI-DRAN0339-GEIG-KUNDE-NR       PIC S9(18)V COMP-3.
             10 PI-DRAN0339-GEIG-LKZ            PIC  X(0003).
             10 PI-DRAN0339-GEIG-NAME           PIC  X(0026).
             10 PI-DRAN0339-GEIG-NAME-K-INH     PIC  X(0026).
             10 PI-DRAN0339-GEIG-OKI-ZEITPUNKT  PIC  X(0026).
             10 PI-DRAN0339-GEIG-ORT            PIC  X(0030).
             10 PI-DRAN0339-GEIG-PIN            PIC S9(9) COMP.
             10 PI-DRAN0339-GEIG-PLZ            PIC  X(0006).
             10 PI-DRAN0339-GEIG-STEUERNUMMER   PIC  X(0013).
             10 PI-DRAN0339-GEIG-STRASSE        PIC  X(0045).
             10 PI-DRAN0339-GEIG-TELEFON        PIC  X(0030).
             10 PI-DRAN0339-GEIG-TITEL          PIC  X(0020).
             10 PI-DRAN0339-GEIG-VERSION        PIC  X(0004).
             10 PI-DRAN0339-GEIG-VM             PIC S9(9) COMP.
             10 PI-DRAN0339-GEIG-VM-PZ          PIC  X(0001).
             10 PI-DRAN0339-GEIG-VNAME-K-INH    PIC  X(0026).
             10 PI-DRAN0339-GEIG-VORNAME        PIC  X(0026).
      *
       01  PO-DRAN0339.
           05  PO-DRAN0339-RC                    PIC  X(003).
               88 PO-DRAN0339-RC-OK              VALUE 'OK '.
               88 PO-DRAN0339-RC-DUPLICATE-VALUE VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************

