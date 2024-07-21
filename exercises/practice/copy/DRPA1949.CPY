      *AUTHOR.        M.SCHEIDEL.
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  ! AENDERUNG                       ! NAME
      * ---------------------------------------------------------------
      * DSY03269!19.01.21! NEU                             ! SCHEIDEL
      *         !        !                                 !
      *         !        !                                 !
      *         !        !                                 !
      *
      * ENDE-LOGBUCH
      *****************************************************************
      *****************************************************************
      *
      *   ABLEITUNG VERARBEITUNGSKENNZEICHEN
      *   - AUS DEM UEBERGEBENEN NUMMERISCHEN WERT WERDEN DIE 
      *     EINZELNEN VERARBEITUNGSKENNZEICHEN EXTRAHIERT UND
      *     IN DER FUNKTIONSAUSGABE ZUR VERFUEGUNG GESTELLT
      *
      *****************************************************************
      *
        01 PI-DRPA1949.
           05 PI-DRPA1949-DATEN.
              10 PI-DRPA1949-PGEB-GES       PIC S9(004) BINARY.
              10 PI-DRPA1949-VERARB-KZ      PIC  9(004).
      *
        01 PO-DRPA1949.
           05 PO-DRPA1949-RC                PIC  X(003).
              88 PO-DRPA1949-RC-OK               VALUE 'OK '.
              88 PO-DRPA1949-RC-NOT-OK           VALUE 'NOK'.
              88 PO-DRPA1949-RC-GES-UNZUL        VALUE 'GUN'.
           05 PO-DRPA1949-DATEN.
              10 PO-DRPA1949-ALLE-KZ.
                 15 PO-DRPA1949-KZ-VOR      PIC  X(001).
                    88 PO-DRPA1949-KZ-VOR-JA     VALUE 'J'.
                    88 PO-DRPA1949-KZ-VOR-NEIN   VALUE 'N'.            
                 15 PO-DRPA1949-KZ-EAN      PIC  X(001).
                    88 PO-DRPA1949-KZ-EAN-JA     VALUE 'J'.
                    88 PO-DRPA1949-KZ-EAN-NEIN   VALUE 'N'.           
                 15 PO-DRPA1949-KZ-PNL      PIC  X(001).
                    88 PO-DRPA1949-KZ-PNL-JA     VALUE 'J'.
                    88 PO-DRPA1949-KZ-PNL-NEIN   VALUE 'N'.            
                 15 PO-DRPA1949-KZ-PDO      PIC  X(001).
                    88 PO-DRPA1949-KZ-PDO-JA     VALUE 'J'.
                    88 PO-DRPA1949-KZ-PDO-NEIN   VALUE 'N'.             
                 15 PO-DRPA1949-KZ-05       PIC  X(001).
                    88 PO-DRPA1949-KZ-05-JA      VALUE 'J'.
                    88 PO-DRPA1949-KZ-05-NEIN    VALUE 'N'.             
                 15 PO-DRPA1949-KZ-06       PIC  X(001).
                    88 PO-DRPA1949-KZ-06-JA      VALUE 'J'.
                    88 PO-DRPA1949-KZ-06-NEIN    VALUE 'N'.             
                 15 PO-DRPA1949-KZ-07       PIC  X(001).
                    88 PO-DRPA1949-KZ-07-JA      VALUE 'J'.
                    88 PO-DRPA1949-KZ-07-NEIN    VALUE 'N'.             
                 15 PO-DRPA1949-KZ-08       PIC  X(001).
                    88 PO-DRPA1949-KZ-08-JA      VALUE 'J'.
                    88 PO-DRPA1949-KZ-08-NEIN    VALUE 'N'.             
                 15 PO-DRPA1949-KZ-09       PIC  X(001).
                    88 PO-DRPA1949-KZ-09-JA      VALUE 'J'.
                    88 PO-DRPA1949-KZ-09-NEIN    VALUE 'N'.             
                 15 PO-DRPA1949-KZ-10       PIC  X(001).
                    88 PO-DRPA1949-KZ-10-JA      VALUE 'J'.
                    88 PO-DRPA1949-KZ-10-NEIN    VALUE 'N'.             
                 15 PO-DRPA1949-KZ-11       PIC  X(001).
                    88 PO-DRPA1949-KZ-11-JA      VALUE 'J'.
                    88 PO-DRPA1949-KZ-11-NEIN    VALUE 'N'.             
                 15 PO-DRPA1949-KZ-12       PIC  X(001).
                    88 PO-DRPA1949-KZ-12-JA      VALUE 'J'.
                    88 PO-DRPA1949-KZ-12-NEIN    VALUE 'N'.
                 15 PO-DRPA1949-KZ-13       PIC  X(001).
                    88 PO-DRPA1949-KZ-13-JA      VALUE 'J'.
                    88 PO-DRPA1949-KZ-13-NEIN    VALUE 'N'.
              10 PO-DRPA1949-KZ-TAB 
                 REDEFINES PO-DRPA1949-ALLE-KZ.
                 15 PO-DRPA1949-KZ          OCCURS 13
                                            INDEXED BY IX-DRPA1949-KZ
                                            PIC  X(001).
                    88 PO-DRPA1949-KZ-JA         VALUE 'J'.
                    88 PO-DRPA1949-KZ-NEIN       VALUE 'N'.
